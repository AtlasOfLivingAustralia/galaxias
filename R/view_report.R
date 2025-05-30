#' @rdname check_archive
#' @param x An object of class `gbif_validator`.
#' @order 3
#' @export
view_report <- function(x, n = 5){
  if(missing(x)){
    cli::cli_abort("Please supply an object to `view_report()`")
  }
  if(!inherits(x, "gbif_validator")){
    cli::cli_abort("Argument {.arg x} must have class `gbif_response`.")
  }
  print_archive_POST(x)
  print_archive_status(x)
  print_archive_issues(x, n = n)
}


#' Internal function to print outcome of `api_gbif_validator_post()`, 
#' which is called within `check_archive()`
#' @noRd
#' @keywords Internal
print_archive_POST <- function(x){
  date_created <- as.POSIXlt(x$created, 
                             format = c("%Y-%m-%dT%H:%M:%S"))
  cli::cli_text("File {.file {x$file}} submitted to GBIF validator API by user `{x$username}` at {date_created}.")
  cli::cat_line()
  cli::cli_text("Key: \"{x$key}\"")
  cli::cli_bullets(c(i = "Please retain this key to recheck your results using `get_report()`"))
  cli::cat_line()
}


#' Internal function to print status
#' @noRd
#' @keywords Internal
print_archive_status <- function(x){
  status <- tolower(x$status)
  if(status %in% c("downloading", "submitted", "running", "queued")){
    cli::cli_text("Status: {cli::col_blue(status)}")
  }else if(status %in% c("aborted", "failed")){
    cli::cli_text("Status: {cli::col_red(status)}")
  }else if(status == "finished"){
    cli::cli_text("Status: {cli::col_green(status)}")
  }else{
    cli::cli_text("Status: {status}") # shouldn't ever happen
  }
}


#' Internal function to print issues from `get_report()`, 
#' @noRd
#' @keywords Internal
print_archive_issues <- function(x, n){
  files <- purrr::pluck(x, "metrics", "files")
  if(!is.null(files)){
    n_files <- length(files)
    n_issues <- purrr::map(files, 
                           \(a){
                             length(a$issues)
                           }) |>
      unlist() |>
      sum()
    cli::cli_text("Found {n_issues} issues in {n_files} files:") 
    purrr::map(files,
               \(a){print_file(a, n = n)}) |>
      invisible() # necessary to prevent printing objects to the console
  }
}


#' Internal function to print issues for a single file
#' @noRd
#' @keywords Internal
print_file <- function(x, n){
  file_name <- get_validator_file_name(x)
  cli::cat_line()
  if(!is.null(x$fileType)){
    cli::cli_text("File \"{file_name}\" ({x$file_type})")
  }else{
    cli::cli_text("File \"{file_name}\"")
  }
  issues_vec <- seq_len(min(n, length(x$issues)))
  purrr::map(x$issues[issues_vec],
             print_issue) |>
    invisible() # necessary to prevent printing objects to the console
}


#' Internal function to extract file name from GBIF validator object
#' @noRd
#' @keywords Internal
get_validator_file_name <- function(x){
  if(!is.null(x$fileName)){
    x$fileName
  }else if(!is.null(x$rowType)){
    x$rowType
  }else{
    "<unknown>"
  }
}


#' Internal function to print issues for a single issue
#' @noRd
#' @keywords Internal
print_issue <- function(x){
  result <- list(category =  purrr::pluck(x, "issueCategory"),
                 issue = to_lower_case(purrr::pluck(x, "issue")),
                 message = purrr::pluck(x, 
                                        !!!list("samples", 1, "relatedData", 1),
                                        .default = ""))
  if(result$message == ""){
    cli::cli_bullets(c("*" = "{result$issue}"))
  }else{
    cli::cli_bullets(c("*" ="{result$message} ({result$issue})"))
  }
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