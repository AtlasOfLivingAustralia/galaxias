#' Functions to check a tibble for compatability with DwC format
#' 
#' This set of functions looks for mandatory or recommended columns and informs
#' the user of the result. In some cases they also update the tibble with new
#' data
#' @param data A tibble
#' @rdname check-dwc
#' @importFrom rlang abort
check_unique_identifiers <- function(data){
  column_names <- colnames(data)
  required_columns <- c("occurrenceID", "catalogueNumber", "recordNumber")
  if(!any(column_names %in% required_columns)){
    # check for unique-like columns
      # if present: rename_identifier()
      # else - build_random_identifier() [OR build_composite_identifier()?]
    
    #     bullets <- c(
    #   "Missing required columns.",
    #   i = glue("Darwin Core standards require that one of 
    #            `occurrenceID`, `catalogueNumber` or `recordNumber`
    #            columns are supplied.")
    # )
    # abort(bullets, call = caller_env())
    # Note: option here to add exception to looks for columns whose 
    # length(unique(tibble$column)) == nrow(tibble) and assign these as 
    # `occurrenceID`
  }else{
    data
  }
}

#' @rdname check-dwc
#' @importFrom rlang abort
check_mandatory_fields <- function(data){
  column_names <- colnames(data)
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
    data
  }
}

#' @rdname check-dwc
#' @importFrom rlang inform
check_recommended_fields <- function(data){
  column_names <- colnames(data)
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
  return(data)
}

#' @rdname check-dwc
#' @importFrom crayon green red
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom rlang inform
check_percent_match <- function(data){
  column_names <- colnames(data)
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
  
  return(data)
}