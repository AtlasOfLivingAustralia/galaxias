#' Add an `occurrenceID` field to a `tibble`
#' 
#' Every occurrence should have an occurrenceID entry. Ideally these should be 
#' persistent to avoid being lost in future updates. They should also be 
#' unique, both within the dataset, and (ideally) across all other datasets.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param ... Optional fields to use as a unique identifier. If multiple are
#' provided, will be concatenated into a single unique identifier.
#' @returns A `tibble` with an attached `basisOfRecord` field
#' @export
use_occurrenceID <- function(.df,
                             ...){
  # mc <- match.call(expand.dots = FALSE)
  # mc$...
  
  dots <- list(...)
  
  browser()
  if(length(dots) > 0){
    .df |>
      mutate(occurrenceID = build_composite_identifier(.df, dots))
  }else{
    .df |>
      mutate(occurrenceID = use_id_random(.df)) ##TODO: This doesn't work
  }
  check_occurrenceID(df, level = "abort")
}




