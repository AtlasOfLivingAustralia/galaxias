#' Print objects returned by APIs
#' 
#' Currently only `validate_archive()`
#' @param x An object to print.
#' @param n Number of entries to print
#' @param \dots Additional arguments, currently ignored.
#' @name print_validation
#' @importFrom cli cli_h2
#' @importFrom cli cat_line
#' @export
print.gbif_validator_response <- function(x, n = 5, ...){
  
  # summary information first  
  cli_h2("Validation report")
  cat_line(glue("File \"{x$file}\" uploaded to \'api.gbif.org.au/v1/validation\'"))
  cat_line(glue("by user \'{x$username}\' on {x$created}"))
  if(x$status != "FAILED"){
    cat_line(glue("Status: {col_green(x$status)}"))
  }else{
    print_validator_issues(x, n = n)
  }
}

#' Internal function to print all issues returned by API
#' @importFrom cli cli_h3
#' @importFrom dplyr slice_head
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
print_validator_issues <- function(x, n){
  # extract useful information
  files <- pluck(x, "metrics", "files")
  issues_tibble <- get_combined_validator_issues_tibble(files)
  n_issues <- nrow(issues_tibble)
  
  # print
  cat_line(glue("Status: {col_red(x$status)}"))
  cat_line(glue("Report: {n_issues} issues found in {length(files)} files"))
  cat_line()
  if(n_issues > n){
    cli_h3(glue("First {n} issues:"))
    cat_line("To increase, use `print(x, n = number_of_issues)`")
  }else{
    cli_h3("Issues")
  }
  issues_tibble |>
    slice_head(n = n) |>
    print_validator_issues_tibble()
}

#' Internal function to get tibble of issues
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
get_combined_validator_issues_tibble <- function(x){
  map(x, \(a){
    result <- get_single_validator_issues_tibble(a) |>
      mutate(file = get_validator_file_name(a), .before = 1)
    result
  }) |>
    bind_rows()
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
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
get_single_validator_issues_tibble <- function(x){
  map(x$issues, parse_gbif_file_issues) |>
    bind_rows() |>
    mutate(category = to_lower_case(.data$category),
           issue = to_lower_case(.data$issue),
           severity = to_lower_case(.data$severity))
}

#' Internal function to parse a single issue to a one-row tibble
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
parse_gbif_file_issues <- function(a){
  b <- unlist(a)
  tibble(category = a$issueCategory,
         issue = a$issue,
         severity = {str_extract(names(b[length(b)]), "\\.[:upper:]+$") |>
             str_remove("^\\.")},
         message = b[length(b)])
}

#' Internal function to show each entry in a tibble
#' @importFrom cli cat_line
#' @importFrom glue glue
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
print_validator_issues_tibble <- function(.df){
  map(
    split(.df, seq_len(nrow(.df))),
    \(a){
      cat_line()
      cat_line(
        glue("Category: {a$category} | Issue: {a$issue} | Severity: {a$severity}"))
      cat_line(a$message)
      cat_line()
    }) |>
    invisible()
}

#' Simple case manipulation
#' probably could use more care
#' @importFrom stringr str_replace_all
#' @noRd
#' @keywords Internal
to_lower_case <- function(x){
  x |>
    str_replace_all("_", " ") |>
    tolower()
}