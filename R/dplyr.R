#' Functions masked from `dplyr`
#' 
#' These functions are masked for class `dwc` to support type-specific data
#' checks. Experimental.
#' @name galaxias-dplyr
#' @importFrom dplyr rename
#' @importFrom rlang expr
#' @importFrom rlang set_names
#' @importFrom tidyselect eval_rename
#' @export
mutate.dwc_df <- function(.data, 
                          ..., 
                          .by = NULL,
                          .keep = c("all", "used", "unused", "none"),
                          .before = NULL,
                          .after = NULL
){
  .keep <- match.arg(.keep)
  x <- enquos(...) |>
    names()
  result <- mutate(as_tibble(.data), 
                   ...,
                   .keep = .keep,
                   .before = .before,
                   .after = .after)
  check_dwc(result[, x])
  result
}

# `relocate()` is an option, but not sure that it would say anything useful

#' @rdname galaxias-dplyr
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

#' @rdname galaxias-dplyr
#' @importFrom dplyr rename_with
#' @export
rename_with.dwc_df <- function(.data, 
                               .fn, 
                               .cols = everything(), 
                               ...){
  x <- as_tibble(.data)
  result <- rename_with(x,
                        .fn = .fn,
                        .cols = .cols,
                        ...)
  x_names <- colnames(result)
  new_names <- x_names[!(x_names %in% colnames(x))]
  check_dwc(result[, new_names])
  result
}


#' @rdname galaxias-dplyr
#' @export
select.dwc_df <- function(.data, ...){
  result <- select(.data, ...)
  check_dwc(result)
  result
}