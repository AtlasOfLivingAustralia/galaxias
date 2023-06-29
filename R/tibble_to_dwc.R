#' Convert a tibble to Darwin-Core compatible content and colnames
#'
#' This function takes a tibble of user-supplied data, and seeks to match 
#' supplied column names to those given in the DarwinCore schema. It also 
#' corrects common problems and removes non-DwC-compatible fields.
#' For finer control, see `check()`.
#' @param data a `tibble` containing biological observations
#' @returns A tibble identical to the input, except with 'corrected' column 
#' names.
#' @details
#' This function is in progress and behaviour may change. At present it...
#' @importFrom utils menu
#' @importFrom tibble tibble
#' @importFrom janitor clean_names
# @importFrom emo ji_find
#' @importFrom glue glue glue_collapse
#' @importFrom readr read_csv
#' @export
tibble_to_dwc <- function(data) {

  # check inputs
  if(missing(data)){
    abort("`data` is missing, with no default")
  }
  
  # pipe all `check()` functions in sequence
  # note: all accept and return a tibble
  data |> 
    rename_camel_case() |>
    check_unique_identifiers() |>
      # search for unique identifiers if not supplied
      # add new function build_unique_identifiers(from_columns, random, sequential)
    check_mandatory_fields() |>
    check_recommended_fields() |>
    check_percent_match()
  
  # check scientific names - detect languages? 2 words with particular suffixes?
  # check_dates
  # check_places
  # check_country_codes
  
  # possible outputs:
    # report showing e.g. field names, missing values etc, set.seed(n)
    # code to regenerate that DWCA from the provided inputs
  
  # option to check against ALA name matching (search_taxa())
  
  # stuff to ask questions about:
    # spatial precision
    # all one set of observers? Or different observers per row?
  
  
  # Below code still needs reformatting to above style
  dwc_terms <- dwc_terms_archived$column_name
  colnames(data) <- user_col_names # q: is this sensible at this point?

  # Identify and rename incorrectly formatted columns
  if(any(user_col_names %in% dwc_terms) & any(user_col_names != names(data))) {

    matched_cols <- names(data[, user_col_names %in% dwc_terms]) |> sort() # TODO: Uses alphabetical order to match cols. This is hacky. Fix
    correct_names <- dwc_terms[dwc_terms %in% user_col_names] |> sort()

    # ask if user wants to convert column names to DarwinCore case
    rename_q_answer <-
      menu(c("Proceed", "Exit"),
           title = glue("
           ---
           Your columns are not in standard DarwinCore case format.

           We will need reformat matched columns to DarwinCore to proceed.
           "))
    if(rename_q_answer == 1) {
      bullets <- c(
        "Column names changed:",
        glue("{matched_cols} -> {crayon::green(correct_names)}")
      )
      inform(bullets)

      data_dwc_names <- rename_columns(data, matched_cols, correct_names)
      return(data_dwc_names)
    }else{
      return(data)
    }
  }else{
    return(data)
  }
}

#' Internal function to rename columns
#' @noRd
#' @keywords Internal
#' @importFrom dplyr rename_with
rename_columns <- function(data, matched_cols, correct_names) {
    # rename columns
    data_dwc_names <- data |>
      rename_with(~ correct_names[which(matched_cols == .x)], .cols = matched_cols)

    return(data_dwc_names)
}