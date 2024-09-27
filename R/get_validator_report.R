#' @rdname validate_archive
#' @param key A unique identifier key from GBIF
#' @importFrom httr2 request
#' @importFrom httr2 req_options
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom glue glue
#' @order 2
#' @export
get_validator_report <- function(key){
  result <- glue("https://api.gbif.org/v1/validation/{key}") |>
    request() |>
    req_options(
      httpauth = 1,
      userpwd = gbif_username_string()) |>
    req_perform() |>
    resp_body_json()
  class(result) <- c("gbif_validator_response", "list")
  result
}