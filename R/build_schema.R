#' Create a `schema` for a Darwin Core Archive
#' 
#' A schema is an xml document that maps the files and field names in a DwCA.
#' It works by detecting column names on csv files in a specified directory;
#' these should all be Darwin Core terms for this function to produce reliable
#' results.
#' @param source A directory (**not** a file) containing files to be documented 
#' in the schema document. Defaults to the `data` folder within the current 
#' working directory. Note that files that do not match the Darwin Core naming 
#' convention and/or do not end in `.csv` are ignored.
#' @param destination A file name for the resulting schema document. Defaults
#' to `./data/meta.xml` for consistency with the Darwin Core standard.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the specified directory.
#' @importFrom paperbark write_eml
#' @importFrom glue glue
#' @importFrom rlang abort
#' @export
build_schema <- function(source = "data", 
                         destination = "./data/meta.xml") {
  get_default_directory(source) |>
    detect_dwc_files() |>
    detect_dwc_fields() |>
    add_front_matter() |>
    write_eml(file = destination)
  cli::cli_alert_success("Schema successfully built. Saved as {destination}.")
  cli::cli_progress_done()
}

#' Wait time
#' @noRd
#' @keywords Internal
wait <- function(seconds = 1) {
  Sys.sleep(seconds)
}


#' Function progress message
#'
#' @description
#' Informs users about the progress of their ongoing function steps.
#'
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_update
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
    wait(0.0001) # remove zeroes to make messages slower
    cli::cli_progress_update()
  }
  
}

#' Internal function to create core/extension framework for files
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom purrr map
#' @importFrom rlang .data
#' @noRd
#' @keywords Internal
detect_dwc_files <- function(directory){
  progress_update("Detecting Darwin Core files...")
  available_exts <- dwc_extensions()
  supported_files <- available_exts |>
    pull("file")
  available_exts$present <- supported_files |>
    map(\(x) {glue("{directory}/{x}") |> file.exists()}) |> 
    unlist()
  # check whether there are no csvs, and if so, abort
  if(all(!available_exts$present)){
    file_names <- glue_collapse(available_exts$file,
                                sep = ", ",
                                last = " or ")
    bullets <- c(
      glue("Specified directory (\"{directory}\") does not contain any Darwin Core-compliant csv files."),
      i = glue("Accepted names are {file_names}."))
    abort(bullets)
  }
  available_exts |>
    filter(.data$present == TRUE) |>
    mutate(label = c("core", 
                     rep("extension", 
                         length(which(.data$present == TRUE)) - 1)),
           level = 2,
           directory = glue("{directory}")) |>
    select("type", "directory", "file", "level", "label", "attributes")
}

#' Internal function to list metadata for Darwin Core extensions
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
dwc_extensions <- function(){
  tibble(
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

#' Internal function to add field names to tibble
#' @noRd
#' @keywords Internal
detect_dwc_fields <- function(df){
  progress_update("Detecting Darwin Core fields in dataset...")
  split(df, seq_len(nrow(df))) |>
    map(\(x){
      bind_rows(create_schema_row(x),
                create_file_row(x),
                create_id_row(),
                create_field_rows(x))

    }) |>
    bind_rows()
}

#' Internal function to create file name
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
create_schema_row <- function(x){
  x |> 
    mutate(text = NA) |>
    select("level", "label", "text", "attributes")
}

#' Internal function to create file name
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
create_file_row <- function(x){
  tibble(
    level = c(3, 4),
    label = c("files", "location"),
    text = c(NA, x$file),
    attributes = list(NA))
}

#' Internal function to create id field
#' @noRd
#' @keywords Internal
create_id_row <- function(){
  tibble(
    level = 3,
    label = "id",
    text = NA,
    attributes = list(list(index = "0")))
}

#' Internal function to create xml map of column names
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
create_field_rows <- function(x){
  field_names <- glue("{x$directory}/{x$file}") |>
    get_field_names()
  n_fields <- length(field_names)
  # get sequence of indexes
  index_list <- as.list(seq_along(field_names))
  names(index_list) <- rep("index", n_fields)
  # get sequence of urls
  dwc_df <- corella::darwin_core_terms
  term_list <- map(field_names, 
      .f = \(a){
        term_lookup <- dwc_df$term == a
        if(any(term_lookup)){
          dwc_df$url[which(term_lookup)[1]]
        }else{
          "no-dwc-term-found"
        }
      })
  # term_list <- as.list(glue("http://rs.tdwg.org/dwc/terms/{field_names}")) # obsolete
  names(term_list) <- rep("term", n_fields)
  # combine
  tibble(level = 3,
         label = "field",
         text = NA,
         attributes = map(seq_len(n_fields), 
                          \(x){c(index_list[x], term_list[x])}))
}

#' simple function to get column names from a csv
#' @importFrom utils read.csv
#' @noRd
#' @keywords Internal
get_field_names <- function(file){
  read.csv(file, nrows = 1) |>
    colnames()
}

#' Internal function to add `xml` and `archive` sections to tibble
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
add_front_matter <- function(df){
  progress_update("Building xml components...")
  front_row <- tibble(
    level = 1,
    label = "archive",
    text = NA,
    attributes = list(
      list(xmlns = "http://rs.tdwg.org/dwc/text/",
           metadata = "eml.xml")
    )
  )
  bind_rows(front_row, df)
}
