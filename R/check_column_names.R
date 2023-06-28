#' Internal function to check percent match to ALL DwC terms
#' @noRd
#' @keywords Internal
#' @importFrom crayon green red
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom rlang inform
check_percent_match_columns <- function(column_names){
  dwc_terms <- dwc_terms_archived$column_name
  user_total_cols <- length(column_names)
  
  # Check for complete match
  if(all(column_names %in% dwc_terms)) {
    # party <- emo::ji_find("party")$emoji[5]
    inform(glue::glue("{crayon::green('100% of columns match DarwinCore terms')}")) # {party}
  } else {
    n_matched <- sum(column_names %in% dwc_terms)
    prop_matched <- paste(round(n_matched / user_total_cols*100, 1), "%", sep = "")
    unmatched <- column_names[!column_names %in% dwc_terms]
    unmatched_names_list <- glue_collapse(unmatched, sep = ", ", last = " and ")

    bullets <- c(
      glue("{crayon::red(prop_matched)} {crayon::red('of columns match DarwinCore terms')}"),
      x = glue("Unmatched columns: {unmatched_names_list}")
    )
    inform(bullets)
  }
  
  return(column_names)
}

#' Internal function to check whether unique identifier columns exist
#' 
#' Note: option here to add exception to looks for columns whose 
#' length(unique(tibble$column)) == nrow(tibble) and assign these as 
#' `occurrenceID`
#' 
#' Question: what string conversion happens before this point?
#' @noRd
#' @keywords Internal
#' @importFrom rlang abort
check_unique_identifier_columns <- function(column_names){
  if(!any(column_names %in% 
          c("occurrenceID", "catalogueNumber", "recordNumber"))){
    bullets <- c(
      "Missing required columns.",
      i = glue("Darwin Core standards require that one of 
               `occurrenceID`, `catalogueNumber` or `recordNumber`
               columns are supplied.")
    )
    abort(bullets, call = caller_env())
  }else{
    column_names
  }
}

#' Internal function to check whether mandatory columns exist
#' @noRd
#' @keywords Internal
#' @importFrom rlang abort
check_mandatory_columns <- function(column_names){
  required_cols <- c("scientificName", "eventDate", "basisOfRecord")
  missing_required_fields <- !(required_cols %in% column_names)
  if(any(missing_required_fields)) {
    unmatched <- required_cols[missing_required_fields]
    list_of_unmatched <- glue::glue_collapse(unmatched,
                                             sep = ", ")
    bullets <- c(
      "Missing required columns.",
      i = glue("Darwin Core standards require that
             `scientificName`, `eventDate` & `basisOfRecord`
             columns are supplied."),
      x = glue("Missing column(s): {list_of_unmatched}")
    )
    abort(bullets, call = caller_env())
  }else{
    column_names
  }
}

#' Internal function to check whether recommended columns exist
#' @noRd
#' @keywords Internal
#' @importFrom rlang inform
check_recommended_columns <- function(column_names){
  suggested_cols <- c("kingdom", "taxonRank", 
                      "decimalLatitude", "decimalLongitude", "geodeticDatum",
                      "countryCode", 
                      "individualCount", "organismQuantity", "organismQuantityType")
  if(!all(column_names %in% suggested_cols)) {
    unmatched <- names(data[,!user_col_names %in% suggested_cols])
    list_of_unmatched <- glue::glue_collapse(unmatched,
                                             sep = ", ")
    bullets <- c(
      "Missing suggested columns.",
      i = "Darwin Core standards recommend that the following columns are supplied:",
      i = glue_collapse(suggested_cols, sep = ", ", last = " & ")
    )
    inform(bullets, call = caller_env())
  }
  return(column_names)
}