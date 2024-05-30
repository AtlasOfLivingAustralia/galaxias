#' Add abundance fields to a `tibble`
#' 
#' In some field methods, it is common to observe more than one individual
#' per observation; to observe abundance using non-integer measures such as 
#' mass or area; or to seek individuals but not find them (abundance of zero). 
#' As these approaches use different DwC terms, this function assists in 
#' specifying abundances in a flexible way.
#' @param .df a `data.frame` or `tibble` that the column should be appended to.
#' @param individualCount The number of individuals present
#' @param organismQuantity A number or enumeration value for the quantity
#' @param organismQuantityType The type of quantification system used for `organismQuantity`
#' @param occurrenceStatus whether the taxon in question is `present` or `absent`
#' @returns A tibble with the requested fields (see details).
#' @details
#' This needs some clever behaviour
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @noRd
add_abundance <- function(.df,
                          individualCount = NULL,
                          organismQuantity = NULL,
                          organismQuantityType = NULL,
                          occurrenceStatus = NULL){
  
}