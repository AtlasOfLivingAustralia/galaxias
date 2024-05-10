#' Functions masked from dplyr
#' 
#' These functions are masked for class `dwc` to support type-specific data
#' checks. Experimental
#' @name galaxias-dplyr
#' @importFrom dplyr rename
#' @importFrom rlang expr
#' @importFrom rlang set_names
#' @importFrom tidyselect eval_rename
#' @export
rename.dwc_df <- function(.data, ...){
  loc <- c(...) |>
         expr() |>
         eval_rename(data = as_tibble(.data))
  ## eval_rename() only returns changes, meaning we can use it to see whether
  ## changed fields are DwC compliant
  names <- names(.data)
  names[loc] <- names(loc)
  result <- set_names(.data, names)
  check_dwc(result[, loc])
  result
}