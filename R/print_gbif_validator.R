#' Print objects returned by APIs
#' 
#' Currently only `check_archive()` and `get_report()`, where the 
#' latter is called by the former.
#' @param x An object to print.
#' @param \dots Additional arguments, currently ignored.
#' @name print_report
#' @export
print.gbif_validator_post <- function(x, ...){
  utils::str(x)
  # this is a placeholder until a better print function is written.
}


#' @rdname print_report
#' @param n Number of entries to print
#' @export
print.gbif_validator_response <- function(x, n = 5, ...){
  
  # summary information first  
  cli::cli_h2("Validation report")
  cli::cat_line(glue::glue("Report from \'api.gbif.org.au/v1/validation\' for \'{x$username}\'"))
  cli::cat_line(glue::glue("File: {x$file}"))
  cli::cat_line(glue::glue("Date: {x$created}"))
  cli::cat_line(glue::glue("Key: {x$key}"))
  if(x$status != "FAILED"){
    cli::cat_line(glue::glue("Status: {col_green(x$status)}"))
  }else{
    print_validator_issues(x, n = n)
  }
}

#' Internal function to print all issues returned by API
#' @noRd
#' @keywords Internal
print_validator_issues <- function(x, n){
  # extract useful information
  files <- purrr::pluck(x, "metrics", "files")
  issues_tibble <- get_combined_validator_issues_tibble(files)
  n_issues <- nrow(issues_tibble)
  
  # print
  glue::glue("Status: {col_red(x$status)} ({n_issues} issues found in {length(files)} files)") |>
    cli::cat_line()
  cli::cat_line()
  if(n_issues > n){
    glue::glue("First {n} issues:") |>
      cli::cli_h3()
    cli::cat_line("To increase, use `print(x, n = number_of_issues)`")
  }else{
    cli::cli_h3("Issues")
  }
  issues_tibble |>
    dplyr::slice_head(n = n) |>
    print_validator_issues_tibble()
}

#' Internal function to get tibble of issues
#' @noRd
#' @keywords Internal
get_combined_validator_issues_tibble <- function(x){
  purrr::map(x, \(a){
    result <- get_single_validator_issues_tibble(a) |>
      dplyr::mutate(file = get_validator_file_name(a), .before = 1)
    result
  }) |>
    dplyr::bind_rows()
}

#' Internal function to extract file name from GBIF validator object
#' @noRd
#' @keywords Internal
get_validator_file_name <- function(x){
  if(!is.null(x$fileName)){
    file_name <- x$fileName
  }else if(!is.null(x$rowType)){
    file_name <- x$rowType
  }else{
    file_name <- ""
  }
}

#' Internal function to get issues as a tibble
#' @importFrom rlang .data
#' @noRd
#' @keywords Internal
get_single_validator_issues_tibble <- function(x){
  purrr::map(x$issues, parse_gbif_file_issues) |>
    dplyr::bind_rows() |>
    dplyr::mutate(category = to_lower_case(.data$category),
                  issue = to_lower_case(.data$issue),
                  severity = to_lower_case(.data$severity))
}

#' Internal function to parse a single issue to a one-row tibble
#' @noRd
#' @keywords Internal
parse_gbif_file_issues <- function(a){
  b <- unlist(a)
  tibble::tibble(category = a$issueCategory,
                 issue = a$issue,
                 severity = {stringr::str_extract(names(b[length(b)]), 
                                                  "\\.[:upper:]+$") |>
                     stringr::str_remove("^\\.")},
                 message = b[length(b)])
}

#' Internal function to show each entry in a tibble
#' @noRd
#' @keywords Internal
print_validator_issues_tibble <- function(.df){
  purrr::map(
    split(.df, seq_len(nrow(.df))),
    \(a){
      cli::cat_line()
      glue::glue("Category: {a$category} | Issue: {a$issue} | Severity: {a$severity}") |>
        cli::cat_line()
      cli::cat_line(a$message)
      cli::cat_line()
    }) |>
    invisible()
}

#' Simple case manipulation
#' probably could use more care
#' @noRd
#' @keywords Internal
to_lower_case <- function(x){
  x |>
    stringr::str_replace_all("_", " ") |>
    tolower()
}