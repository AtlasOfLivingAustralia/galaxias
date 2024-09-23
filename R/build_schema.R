#' Create a `schema` for a Darwin Core Archive
#' 
#' A schema is an xml document that maps the files and field names in a DwCA.
#' It works by detecting column names on csv files in a specified directory;
#' these should all be Darwin Core terms for this function to produce reliable
#' results.
#' @param directory A folder containing Darwin Core data. Defaults to `data`.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the specified directory.
#' @importFrom elm write_elm
#' @importFrom glue glue
#' @export
build_schema <- function(directory = "data") {
  # check if specified `directory` is present
  if(!file.exists(directory)){
    bullets <- c(glue("`{directory}` directory is required, but missing."),
                 i = "use `usethis::use_data()` to add data to your project.")
    abort(bullets,
          call = call)
  }
  directory |>
    detect_dwc_files() |>
    detect_dwc_fields() |>
    add_front_matter() |>
    write_elm(file = glue("{directory}/meta.xml"))
}

#' Internal function to create core/extension framework for files
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
detect_dwc_files <- function(directory){
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
      glue("Specified directory (\"{directory}\") does not contain any dwc-compliant csv files."),
      i = glue("Accepted names are {file_names}"))
    abort(bullets)
  }
  available_exts |>
    filter(present == TRUE) |>
    mutate(label = c("core", rep("extension", length(which(present == TRUE)) - 1)),
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
  term_list <- as.list(glue("http://rs.tdwg.org/dwc/terms/{field_names}"))
  names(term_list) <- rep("term", n_fields)
  # combine
  tibble(level = 3,
         label = "field",
         text = NA,
         attributes = map(seq_len(n_fields), 
                          \(x){c(index_list[x], term_list[x])}))
}

#' simple function to get column names from a csv
#' @noRd
#' @keywords Internal
get_field_names <- function(file){
  read.csv(file, 
           nrows = 1) |>
    colnames()
}

#' Internal function to add `xml` and `archive` sections to tibble
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
add_front_matter <- function(df){
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