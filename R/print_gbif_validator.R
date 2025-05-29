#' Print objects returned by APIs
#' 
#' Currently only `check_archive()` and `get_report()`, where the 
#' latter is called by the former.
#' @param x An object to print.
#' @param \dots Additional arguments, currently ignored.
#' @name print_report
#' @export
print.gbif_validator_post <- function(x, ...){
  default_gbif_print(x)
  cli::cli_text("Status: {cli::col_blue(tolower(x$status)})")
}

#' @rdname print_report
#' @param n Number of entries to print
#' @export
print.gbif_validator_response <- function(x, n = 5, ...){
  default_gbif_print(x)
  files <- purrr::pluck(x, "metrics", "files")
  if(!is.null(files)){
    cli::cli_text("Status: {cli::col_red(tolower(x$status))}")
    print_validator_issues(files, n = n)  
  }else{ # this could be no issues OR not finished; check
    cli::cli_text("Status: {cli::col_green(tolower(x$status))}")
    cli::cli_text("No issues found!")
  }
}

#' Internal function to improve consistency among functions for handling GBIF outcomes
#' @noRd
#' @keywords Internal
default_gbif_print <- function(x){
  date_created <- as.POSIXlt(x$created, 
                             format = c("%Y-%m-%dT%H:%M:%S"))
  cli::cli_text("File {.file {x$file}} submitted to GBIF validator API by user `{x$username}` at {date_created}.")
  cli::cat_line()
  cli::cli_text("Key: \"{x$key}\"")
  cli::cli_bullets(c(i = "Please retain this key to recheck your results using `get_report()`"))
  cli::cat_line()
}

#' Internal function to print all issues returned by API
#' @noRd
#' @keywords Internal
print_validator_issues <- function(files, n){

  issues_tibble <- get_combined_validator_issues_tibble(files)
  n_issues <- nrow(issues_tibble)
  
  # print
  cli::cli_text("Found {n_issues} issues in {length(files)} files:") 
  print_validator_issues_tibble(issues_tibble,
                                n = n)
}

#' Internal function to get tibble of issues
#' @noRd
#' @keywords Internal
get_combined_validator_issues_tibble <- function(x){
  purrr::map(x, \(a){
    result <- get_single_validator_issues_tibble(a) |>
      dplyr::mutate(file = a$fileName,
                    file_type = tolower(a$fileType),
                    .before = 1)
    result
  }) |>
    dplyr::bind_rows()
}

#' Internal function to extract file name from GBIF validator object
#' @noRd
#' @keywords Internal
# get_validator_file_name <- function(x){
#   if(!is.null(x$fileName)){
#     file_name <- x$fileName
#   }else if(!is.null(x$rowType)){
#     file_name <- x$rowType
#   }else{
#     file_name <- ""
#   }
# }
## Above removed as appears irrelevant, but not certain yet

#' Internal function to get issues as a tibble
#' @importFrom rlang .data
#' @noRd
#' @keywords Internal
get_single_validator_issues_tibble <- function(x){
  purrr::map(x$issues, parse_gbif_file_issues) |>
    dplyr::bind_rows() |>
    dplyr::mutate(category = to_lower_case(.data$category),
                  issue = to_lower_case(.data$issue))
                  # severity = to_lower_case(.data$severity))
}

#' Internal function to parse a single issue to a one-row tibble
#' @noRd
#' @keywords Internal
parse_gbif_file_issues <- function(a){
  tibble::tibble(category =  purrr::pluck(a, "issueCategory"),
                 issue = purrr::pluck(a, "issue"),
                 message = purrr::pluck(a, 
                                        !!!list("samples", 1, "relatedData", 1),
                                        .default = ""))
}

#' Internal function to show each entry in a tibble
#' @noRd
#' @keywords Internal
print_validator_issues_tibble <- function(.df, n){
  df_list <- split(.df, .df$file)
  purrr::map(df_list, \(a){
    cli::cat_line()
    cli::cli_text("File \"{a$file[1]}\" ({a$file_type[1]})")
    file_list <- split(a, seq_len(nrow(a))) 
    rows <- seq_len(min(n, length(file_list)))
    purrr::map(file_list[rows],
               \(b){
                 if(b$message == ""){
                   cli::cli_bullets(c("*" = "{b$issue}"))
                 }else{
                   cli::cli_bullets(c("*" ="{b$issue} ({b$message})"))  
                 }
                 
               })
  }) |>
    invisible()
}

#' Simple case manipulation
#' probably could use more care
#' @noRd
#' @keywords Internal
to_lower_case <- function(x){
  x |>
    gsub("_", " ", x = _) |>
    tolower()
}