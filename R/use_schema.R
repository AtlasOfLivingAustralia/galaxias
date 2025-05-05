#' Create a `schema` for a Darwin Core Archive
#' 
#' @description
#' A schema is an xml document that maps the files and field names in a DwCA. 
#' This map makes it easier to reconstruct one or more related datasets so that 
#' information is matched correctly. It works by detecting column names on csv 
#' files in a specified directory; these should all be Darwin Core terms for 
#' this function to produce reliable results.
#' @param source A folder containing one or more data csv files 
#' `occurrences.csv`, `events.csv` or `multimedia.csv`. Defaults to `data-publish/`.
#' @param destination A file name for the resulting schema document. Defaults
#' to `meta.xml` for consistency with the Darwin Core standard. Note this
#' file is placed within the working directory specified by [galaxias_config()].
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a schema file in the specified directory.
#' @export
use_schema <- function(source = "data-publish",
                       destination = "meta.xml") {
  cli::cli_alert_info("Building schema")
  
  directory <- potions::pour("directory", 
                             .pkg = "galaxias")
  usethis::use_directory(directory)
  
  # detect files
  files <- detect_dwc_files(directory)
  
  # build schema wireframe in a tibble 
  wireframe <- add_dwc_cols(files)
  
  # front matter
  schema <- add_front_matter(wireframe)
  
  cli::cli_alert_success("Writing {.file {destination}}.")
  schema |>
    delma::write_eml(file = destination)
  cli::cli_progress_done()
}

#' Function progress message
#'
#' Informs users about the progress of their ongoing function steps.
#' @noRd
#' @keywords Internal
progress_update <- function(message) {
  cli::cli_progress_step(
    paste0(
      message
    ),
    spinner = TRUE
  )
  
  for (i in 1:100) {
    wait(0.0001) # remove zeroes to make messages slower.
    cli::cli_progress_update()
  }
  
}

#' Wait time
#' @noRd
#' @keywords Internal
wait <- function(seconds = 1) {
  Sys.sleep(seconds)
}

#' Internal function to create core/extension framework for files
#' @importFrom rlang .data
#' @noRd
#' @keywords Internal
detect_dwc_files <- function(directory){
  progress_update("Detecting files..."); wait(.1)
  available_exts <- dwc_extensions()
  supported_files <- available_exts |>
    dplyr::pull("file")
  
  # check if files exist, format result
  available_exts <- available_exts |>
    dplyr::mutate(
      present = glue::glue("{directory}/{supported_files}") |>
        purrr::map(\(file_name)
                   file.exists(file_name)) |>
        unlist(),
      present_formatted = present |>
        purrr::map_chr(\(file_exists) 
                       ifelse(isTRUE(file_exists), 
               cli::symbol$tick |> cli::col_green(), 
               cli::symbol$cross |> cli::col_red()
               )
      )
    )
  
  available_exts$present |> purrr::map_lgl(isTRUE)
  
  # message
  file_check_message(available_exts, "occurrences.csv")
  file_check_message(available_exts, "events.csv")
  file_check_message(available_exts, "multimedia.csv")
  
  # check whether there are no csvs, and if so, abort
  if(all(!available_exts$present)){
    file_names <- cli::ansi_collapse(available_exts$file,
                                sep = ", ",
                                last = " or ")
    bullets <- c(
      "Must include at least one Darwin Core-compliant csv file in {.file {directory}}.",
      i = "Accepted files are {.file {file_names}}.",
      i = "Use `use_data()` to save standardised data in the correct file location.") |>
      cli::cli_bullets() |>
      cli::cli_fmt()
    cli::cli_abort(bullets)
  }
  available_exts |>
    dplyr::filter(.data$present == TRUE) |>
    dplyr::mutate(label = c("core", 
                            rep("extension", 
                                length(which(.data$present == TRUE)) - 1)),
                  level = 2,
                  directory = glue::glue("{directory}")) |>
    dplyr::select("type", "directory", "file", "level", "label", "attributes")
}

#' Internal function to list metadata for Darwin Core extensions
#' @noRd
#' @keywords Internal
dwc_extensions <- function(){
  tibble::tibble(
    type = c("event",
             "occurrence", 
             "multimedia"),
    file = c("events.csv",
             "occurrences.csv",
             "multimedia.csv"),
    attributes = list(
      list(encoding="UTF-8",
           rowType="http://rs.gbif.org/terms/Event",
           fieldsTerminatedBy=",",
           linesTerminatedBy="\r\n",
           fieldsEnclosedBy="&quot;",
           ignoreHeaderLines="1"),
      list(encoding="UTF-8",
           rowType="http://rs.tdwg.org/dwc/terms/Occurrence",
           fieldsTerminatedBy=",",
           linesTerminatedBy="\r\n",
           fieldsEnclosedBy="&quot;",
           ignoreHeaderLines="1"),
      list(encoding="UTF-8",
           rowType="http://rs.gbif.org/terms/1.0/Multimedia",
           fieldsTerminatedBy=",",
           linesTerminatedBy="\r\n",
           fieldsEnclosedBy="&quot;",
           ignoreHeaderLines="1")
    )
  )
}

#' Format message for detecting if a file is present or not
#' @noRd
#' @keywords Internal
file_check_message <- function(file_df, file_name) {
  
  # length of longest file name
  files_nchar <- file_df |>
    dplyr::pull("file") |>
    cli::ansi_nchar() |>
    max()
  
  # build message
  paste0("  ", cli::col_cyan(cli::symbol$bullet), " ", 
         cli::ansi_align(cli::col_blue(glue::glue("{file_df[file_df$file == file_name,]$file }")), files_nchar), " ",
         cli::ansi_align(glue::glue("{file_df[file_df$file == file_name,]$present_formatted}"), cli::ansi_nchar(4))
  ) |>
    cli::cat_line()
  
  wait(.2) # delay next message
  
}

#' Internal function to add column names to tibble
#' @noRd
#' @keywords Internal
add_dwc_cols <- function(df){
  progress_update("Formatting Darwin Core terms...")
  split(df, seq_len(nrow(df))) |>
    purrr::map(\(x){
      dplyr::bind_rows(create_schema_row(x),
                       create_file_row(x),
                       create_id_row(),
                       create_field_rows(x))

    }) |>
    dplyr::bind_rows()
}

#' Internal function to create file name
#' @noRd
#' @keywords Internal
create_schema_row <- function(x){
  x |> 
    dplyr::mutate(text = NA) |>
    dplyr::select("level", "label", "text", "attributes")
}

#' Internal function to create file name
#' @noRd
#' @keywords Internal
create_file_row <- function(x){
  tibble::tibble(
    level = c(3, 4),
    label = c("files", "location"),
    text = c(NA, x$file),
    attributes = list(NA))
}

#' Internal function to create id field
#' @noRd
#' @keywords Internal
create_id_row <- function(){
  tibble::tibble(
    level = 3,
    label = "id",
    text = NA,
    attributes = list(list(index = "0")))
}

#' Internal function to create xml map of column names
#' @noRd
#' @keywords Internal
create_field_rows <- function(x){
  field_names <- glue::glue("{x$directory}/{x$file}") |>
    get_field_names()
  n_fields <- length(field_names)
  # get sequence of indexes
  index_list <- as.list(seq_along(field_names))
  names(index_list) <- rep("index", n_fields)
  # get sequence of urls
  dwc_df <- corella::darwin_core_terms
  term_list <- purrr::map(field_names, 
      .f = \(a){
        term_lookup <- dwc_df$term == a
        if(any(term_lookup)){
          dwc_df$url[which(term_lookup)[1]]
        }else{
          "no-dwc-term-found"
        }
      })
  names(term_list) <- rep("term", n_fields)
  # combine
  tibble::tibble(level = 3,
                 label = "field",
                 text = NA,
                 attributes = purrr::map(seq_len(n_fields), 
                                         \(x){c(index_list[x], term_list[x])}))
}

#' simple function to get column names from a csv
#' @noRd
#' @keywords Internal
get_field_names <- function(file){
  utils::read.csv(file, nrows = 1) |>
    colnames()
}

#' Internal function to add `xml` and `archive` sections to tibble
#' @noRd
#' @keywords Internal
add_front_matter <- function(df){
  progress_update("Building xml components...")
  front_row <- tibble::tibble(
    level = 1,
    label = "archive",
    text = NA,
    attributes = list(
      list(xmlns = "http://rs.tdwg.org/dwc/text/",
           metadata = "eml.xml")
    )
  )
  dplyr::bind_rows(front_row, df)
}
