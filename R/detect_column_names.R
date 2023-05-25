#' Detects DarwinCore column names
#'
#' @import rlang
#' @importFrom utils menu
#' @importFrom tibble tibble
#' @importFrom snakecase to_lower_camel_case
#' @importFrom emo ji_find
#' @importFrom crayon green red blurred
#' @importFrom glue glue glue_collapse
#' @importFrom readr read_csv
#' @export

detect_column_names <- function(data) {

  dwc_terms <- dwc_terms_archived$column_name

  user_col_names <- names(data) |> to_lower_camel_case(abbreviations = c("ID"))
  user_total_cols <- ncol(data)

  if(all(user_col_names %in% dwc_terms)) {
    party <- emo::ji_find("party")$emoji[5]
    inform(glue::glue("{crayon::green('100% of columns match DarwinCore terms')} {party}"))
  }
  else {
    n_matched <- sum(user_col_names %in% dwc_terms)
    prop_matched <- paste(round(n_matched / user_total_cols*100, 1), "%", sep = "")
    unmatched <- names(data[,!user_col_names %in% dwc_terms])
    unmatched_names_list <- glue_collapse(unmatched, sep = ", ", last = " and ")

    bullets <- c(
      glue("{crayon::red(prop_matched)} {crayon::red('of columns match DarwinCore terms')}"),
      x = glue("Unmatched columns: {unmatched_names_list}")
    )
    inform(bullets)
  }
  # browser()
  if(any(user_col_names %in% dwc_terms) & any(user_col_names != names(data))) {

    matched_cols <- names(data[, user_col_names %in% dwc_terms]) |> sort() # TODO: Uses alphabetical order to match cols. This is hacky. Fix
    correct_names <- dwc_terms[dwc_terms %in% user_col_names] |> sort()

    # ask if user wants to convert column names to DarwinCore case
    rename_q_answer <-
      menu(c("Yes", "No"),
           title = glue("
           ---
           Your columns are not in standard DarwinCore case format.

           Reformat matched columns to DarwinCore?
           "))
    if(rename_q_answer == 1) {
      bullets <- c(
        "Column names changed:",
        glue("{matched_cols} -> {crayon::green(correct_names)}")
      )
      inform(bullets)

      data_dwc_names <- rename_columns(data, matched_cols, correct_names)
      return(data_dwc_names)
    }
  }

}

rename_columns <- function(data, matched_cols, correct_names) {
    # rename columns
    data_dwc_names <- data |>
      rename_with(~ correct_names[which(matched_cols == .x)], .cols = matched_cols)

    return(data_dwc_names)

}

