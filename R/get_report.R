#' @rdname check_archive
#' @param key A unique identifier key from GBIF
#' @order 2
#' @export
get_report <- function(key){
  result <- glue::glue("https://api.gbif.org/v1/validation/{key}") |>
    httr2::request() |>
    httr2::req_options(
      httpauth = 1,
      userpwd = gbif_username_string()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  class(result) <- c("gbif_validator_response", "list")
  result
}