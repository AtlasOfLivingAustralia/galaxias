#' Check for Darwin Core field conformance
#' 
#' Function to check whether a data.frame or tibble conforms to DwC standards
#' NOTE: Option to use `bdc` for checks, and/or `pointblank` for running them
#' @param df A tibble against which checks should be run
#' @importFrom rlang inform
#' @importFrom cli cli_bullets
#' @param df a tibble containing data
#' @order 1
#' @export
check_dwc <- function(.df){
  # dwc_terms
  fields <- colnames(.df)
  available_checks <- c("occurrenceID",
                        "basisOfRecord",
                        "decimalLatitude",
                        "decimalLongitude")
  checkable_fields <- fields[fields %in% available_checks]
  check_functions <- c("check_fields",
                       glue("check_{checkable_fields}"))
  dwc_spinny_message()
  # cli_bullets(c(">" = "Checking DwC fields."))
  # run each function on df
  lapply(check_functions, 
         function(x){do.call(x, args = list(.df = .df))}) |>
    invisible()
}

#' Check for non DwC fields
#' NOTE: should probably swap to check_contains() for this.
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom rlang warn
#' @rdname check_dwc
#' @order 2
#' @export
check_fields <- function(.df, 
                         level = c("inform", "warn", "abort")){
  level <- match.arg(level)
  result <- tibble(dwc_terms = colnames(.df)) |>
    check_contains_terms(y = dwc_terms,
                         level = level)
  .df
}

#' Wait time
#' @noRd
#' @keywords Internal
wait <- function(seconds = 1) {
  Sys.sleep(seconds)
}

#' Theatrics
#' @noRd
#' @keywords Internal
dwc_spinny_message <- function(which) {
  
  # define the spinner
  spinny <- cli::make_spinner(
    which = "dots2",
    template = "{spin} Checking DwC fields."
  )
  
  # update the spinner 100 times
  for(i in 1:100) {
    spinny$spin()
    wait(.01)
  }
  
  # clear the spinner from the status bar
  spinny$finish()
}

#' Match DwC terms to column names
#' @param .df vector of values
#' @param y vector against which x should be compared
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_alert_danger
#' @importFrom cli cli_text
#' @importFrom cli cli_bullets
#' @importFrom cli ansi_collapse
#' @importFrom cli cli_div
#' @importFrom cli cli_par
#' @importFrom cli cli_text
#' @importFrom cli cli_end
#' @importFrom cli cli_fmt
#' @importFrom cli col_blue
#' @importFrom cli col_red
#' @importFrom cli col_green
#' @importFrom cli symbol
#' @importFrom cli cat_line
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @importFrom stringr str_pad
#' @noRd
#' @keywords Internal
check_contains_terms <- function(.df, 
                                 y, 
                                 level = "inform",
                                 call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  user_column_names <- .df |> 
    pull(field_name) |>
    unique() |>
    sort()
  name_lookup <- user_column_names %in% y$term
  
  ### All terms
  
  # matches
  matched_values <- user_column_names[name_lookup]
  unmatched_values <- user_column_names[!name_lookup]
  
  # concatenate matched & unmatched fields
  matched_string <- ansi_collapse(glue("{matched_values}"), sep = ", ", last = ", ")
  unmatched_string <- ansi_collapse(glue("{unmatched_values}"), sep = ", ", last = ", ")
  
  # create message components
  result_message <- "Matched {length(matched_values)} of {length(user_column_names)} column name{?s} to DwC terms:"
  matches_message <- c("v" = "Matched: {.field {matched_string}}")
  unmatch_message <- c("x" = "Unmatched: {cli::col_red({unmatched_string})}")
  
  all_cols_match <- rlang::is_empty(unmatched_values)
  
  # build message
  all_terms_message <- function(matches_message, unmatch_message, all_cols_match) {
    cli::cli_h2("All DwC terms")
    cli::cat_line(cli::cli_text(result_message))
    cli::cli_bullets(matches_message)
    cli::cli_bullets(unmatch_message)
    cli::cli_par()
    if(isTRUE(all_cols_match)) {
      # celebrate
      cli::cat_line(paste0("\n", add_emoji(), " ", cli::col_green("All column names matched DwC terms!"), "\n"))
    }
  }
  
  
  ### Minimum required terms
  
  # retrieve column results
  req_terms_results <- check_required_terms(user_column_names)
  
  # create message components
  req_terms_table <- build_req_terms_table(req_terms_results)
  
  all_req_terms_found <- all(req_terms_results$result == "pass")
  
  req_terms_message <- function(table, all_found) {
    cli::cat_line(table)
    if(isTRUE(all_found)) {
      # celebrate
      cli::cat_line(paste0("\n", add_emoji(), " ", cli::col_green("All minimum requirements met!"), "\n"))
    }
  }
  
  
  ## Suggested workflow
  
  # Function matching for suggested workflow
  main_functions <- fn_to_term_table()$main
  other_functions <- fn_to_term_table()$optional
  
  suggested_functions <- main_functions |>
    dplyr::filter(!dwc_term %in% matched_values) |>
    dplyr::distinct(use_function) |>
    pull(use_function)
  
  optional_functions <- other_functions |>
    dplyr::filter(!dwc_term %in% matched_values) |>
    dplyr::distinct(use_function) |>
    pull(use_function)
  
  # add pipe when there are multiple suggested functions
  if(length(suggested_functions) > 1) {
    suggested_functions_piped <- c(paste0(head(suggested_functions, -1), " |> "), tail(suggested_functions, 1))
  } else {
      suggested_functions_piped <- suggested_functions
  }
  
  # add list of optional functions
  if(length(optional_functions) >= 1) {
    optional_functions_string <- ansi_collapse(glue("`{optional_functions}()`"),
                                        sep = ", ",
                                        last = ", ",
                                        trunc = 3)
    optional_functions_message <- cli_text(
      cli::col_grey("Additional functions: {optional_functions_string}")
      ) |> cli_fmt()
  } else {
    optional_functions_message <- ""
  }
  
  workflow_is_empty <- rlang::is_empty(suggested_functions_piped)
  
  # build message
  suggest_message <- function(workflow_is_empty,
                              suggested_functions_piped, 
                              optional_functions_message, 
                              .envir = parent.frame()) {
    if(!any(workflow_is_empty)) {
      cli::cat_line(paste0("\n", "To make your data Darwin Core compliant, use the following workflow:", "\n"))
      cli::cli_text("df |>")
      cli::cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
      lapply(suggested_functions_piped, cli::cli_alert, .envir = .envir)
      cli::cli_end()
      cli::cat_line(paste0("\n", optional_functions_message, "\n"))
    } else {
      cli::cat_line(paste0("\n", add_emoji(), " ", cli::col_green("Your dataframe is Darwin Core compliant!"), "\n"))
      cli::cat_line(paste0("Use your dataframe to build a Darwin Core Archive:\n"))
      cli::cli_text("df |>")
      cli::cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
      lapply(paste0("build_dwca()"), cli::cli_alert, .envir = .envir)
      cli::cat_line()
      cli::cli_end()
    }
  }
  
  
  
  ### Build final message
  full_alert <- function() {
    
    # DwC terms
    cli::cli_div()
    cli::cli_h1("Darwin Core terms")
    all_terms_message(matches_message, 
                      unmatch_message, 
                      all_cols_match)
    
    cli::cli_h2("Minimum required DwC terms")
    req_terms_message(req_terms_table, 
                      all_req_terms_found)
    cli::cli_end()
    
    # Suggested workflow
    cli::cli_h1("Suggested workflow")
    suggest_message(workflow_is_empty, 
                    suggested_functions_piped, 
                    optional_functions_message)
  }
  
  # withr::with_options(
  #   list(cli.width = 80),
  #   custom_alert(suggested_functions_piped, optional_functions)
  # )
  
  full_alert()
  
  .df
}

#' Build message about minimum required terms
#' @noRd
#' @keywords Internal
fn_to_term_table <- function() {

  main <- tibble::tribble(
    ~"use_function",      ~"dwc_term",
    "use_occurrences()", "basisOfRecord",
    "use_occurrences()", "occurrenceID",
    "use_coordinates()", "decimalLatitude",
    "use_coordinates()", "decimalLongitude",
    "use_coordinates()", "geodeticDatum",
    "use_coordinates()", "coordinateUncertaintyInMeters",
    "use_datetime()", "eventDate"
  )
  
  optional <- tibble::tribble(
    ~"use_function", ~"dwc_term",
    "use_locality", "continent", 
    "use_locality", "country",
    "use_locality", "countryCode",
    "use_locality", "stateProvince",
    "use_locality", "locality",
    "use_taxonomy", "kingdom",
    "use_taxonomy", "phylum",
    "use_taxonomy", "class",
    "use_taxonomy", "order",
    "use_taxonomy", "family",
    "use_taxonomy", "genus",
    "use_taxonomy", "species",
    "use_taxonomy", "specificEpiphet",
    "use_taxonomy", "vernacularName"
  )
  
  table <- lst(main, optional) # named list
  
  return(table) 
}




#' Build table for messaging about minimum required terms
#' @importFrom tidyr unnest
#' @importFrom dplyr case_when
#' @importFrom tidyr replace_na 
#' @noRd
#' @keywords Internal
build_req_terms_table <- function(req_terms) {
  
  # Unnest & concatenate terms by group
  missing_results <- req_terms |>
    dplyr::select(-matched) |>
    tidyr::unnest(cols = c(missing)) |>
    dplyr::group_by(term_group) |>
    mutate( # glue names
      missing = cli::ansi_collapse(missing, sep = ", ", last = ", ")
    ) |>
    unique()
  
  matched_results <- req_terms |>
    dplyr::select(-missing) |>
    tidyr::unnest(cols = c(matched)) |>
    dplyr::group_by(term_group) |>
    mutate( # glue names
      matched = cli::ansi_collapse(matched, sep = ", ", last = ", ")
    ) |>
    unique()
  
  req_terms_message <- missing_results |>
    dplyr::full_join(matched_results, 
                     dplyr::join_by(term_group, result)) |>
    # remove other Identifier terms if one or more are matched
    mutate(
      missing = dplyr::case_when(
        term_group == "Identifier (at least one)" & !is.na(matched) ~ NA,
        .default = missing
        )) |>
    # add blank space for correct message formatting
    tidyr::replace_na(list(missing = stringr::str_pad("-", width = 16, side = "right"),
                           matched = stringr::str_pad("-", width = 16, side = "right")))
  
  # Group terms found vs missing
  pass <- req_terms_message |>
    dplyr::filter(result == "pass")
  
  failed <- req_terms_message |>
    dplyr::filter(result == "fail")
  
  pass_group <- glue("{pass$term_group}")
  pass_matched <- glue("{pass$matched}")
  pass_missing <- glue("{pass$missing}")
  fail_group <- glue("{failed$term_group}")
  fail_matched <- glue("{failed$matched}")
  fail_missing <- glue("{failed$missing}")
  
  headers <- paste0(
    "  ",
    cli::ansi_align(cli::col_blue("Type"), max(cli::ansi_nchar(c(pass_group, fail_group)))), " ",
    cli::ansi_align(cli::col_blue("Matched term(s)"), max(cli::ansi_nchar(c(pass_matched, fail_matched)))), " ",
    cli::ansi_align(cli::col_blue("Missing term(s)"), max(cli::ansi_nchar(c(pass_missing, fail_missing)))),"\n",
    collapse = "\n"
  )
  
  bullets_found <- paste0(paste0(
    cli::col_green(cli::symbol$tick), " ", 
    cli::ansi_align(pass_group, max(cli::ansi_nchar(c(pass_group, fail_group)))), " ",
    cli::ansi_align(cli::col_green(pass_matched), max(cli::ansi_nchar(c(pass_matched, fail_matched)))), " ",
    cli::ansi_align(cli::col_red(pass_missing), max(cli::ansi_nchar(c(pass_missing, fail_missing)))), " ",
    collapse = "\n"
  ), "\n")
  
  bullets_missing <- paste0(paste0(
    cli::col_red(cli::symbol$cross), " ",
    cli::ansi_align(fail_group, max(cli::ansi_nchar(c(pass_group, fail_group)))), " ",
    cli::ansi_align(cli::col_green(fail_matched), max(cli::ansi_nchar(c(pass_matched, fail_matched)))), " ",
    cli::ansi_align(cli::col_red(fail_missing), max(cli::ansi_nchar(c(pass_missing, fail_missing)))), " ",
    collapse = "\n"
  ), "\n")
  
  
  # Remove tick when all terms are matched or missing
  if(nrow(pass) == 0) {
    bullets_found <- NULL
  }
  
  if(nrow(failed) == 0) {
    # celebrate
    bullets_missing <- NULL
  }
  
  # final message
  paste0(
    headers, 
    bullets_found, 
    bullets_missing
  ) 
}

#' Minimum required terms for a Darwin Core compliant data archive
#' @noRd
#' @keywords Internal
required_terms <- function() {
  terms <- list(
    identifier = c(
      "occurrenceID",
      "catalogNumber",
      "recordNumber"
    ),
    basis = c(
      "basisOfRecord"
    ),
    name = c(
      "scientificName"
    ),
    location = c(
      "decimalLatitude",
      "decimalLongitude",
      "geodeticDatum",
      "coordinateUncertaintyInMeters"
    ),
    date = c(
      "eventDate"
    )
  )
}

#' Return missing minimum required terms
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
check_required_terms <- function(user_column_names) {
  
  terms <- required_terms()
  
  id <- tibble(
    term_group = "Identifier (at least one)",
    missing = list(terms$identifier[!terms$identifier %in% user_column_names]),
    matched = list(terms$identifier[terms$identifier %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) != 3, "pass", "fail")
    )
  basis <- tibble(
    term_group = "Record type",
    missing = list(terms$basis[!terms$basis %in% user_column_names]),
    matched = list(terms$basis[terms$basis %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )
  name <- tibble(
    term_group = "Scientific name",
    missing = list(terms$name[!terms$name %in% user_column_names]),
    matched = list(terms$name[terms$name %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )
  location <- tibble(
    term_group = "Location",
    missing = list(terms$location[!terms$location %in% user_column_names]),
    matched = list(terms$location[terms$location %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )
  date <- tibble(
    term_group= "Date/Time",
    missing = list(terms$date[!terms$date %in% user_column_names]),
    matched = list(terms$date[terms$date %in% user_column_names])
  ) |>
    mutate(
      result = ifelse(length(missing[[1]]) == 0, "pass", "fail")
    )
  
  # combine
  all_terms <- dplyr::bind_rows(id, basis, name, location, date)
  
  # convert empty row value to NULL
  result <- all_terms |>
    mutate(
      missing = lapply(missing, function(x) if(identical(x, character(0))) NULL else x),
      matched = lapply(matched, function(x) if(identical(x, character(0))) NULL else x)
    ) 
  
  return(result)
}


#' Add happy emoji
#' @noRd
#' @keywords Internal
add_emoji <- function() {
  emoji <- c(
    "\U0001f600", # smile
    "\U0001f973", # party face
    "\U0001f638", # cat grin
    "\U0001f308", # rainbow
    "\U0001f947", # gold medal
    "\U0001f389", # party popper
    "\U0001f38a" # confetti ball
  )
  sample(emoji, 1)
}