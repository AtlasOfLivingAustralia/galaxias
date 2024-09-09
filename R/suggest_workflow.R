#' Check for Darwin Core field conformance
#' 
#' Function to check whether a `data.frame` or `tibble` conforms to Darwin 
#' Core standards. While most users will only want to call `suggest_workflow()`,
#' the underlying check functions are exported for detailed work, or for 
#' debugging.
#' @param df A tibble against which checks should be run
#' @importFrom rlang inform
#' @importFrom cli cli_bullets
#' @returns Invisibly returns the input, but primarily called for the 
#' side-effect of running check functions on that input.
#' @order 1
#' @export
suggest_workflow <- function(.df){
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
  
  # run each function on df
  lapply(check_functions, 
         function(x){do.call(x, args = list(.df = .df))}) |>
    invisible()
  
  invisible(.df)
}

#' Check for non DwC fields
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom rlang warn
#' @rdname check_dwc
#' @order 2
#' @export
check_fields <- function(.df, 
                         level = c("inform", "warn", "abort")){
  level <- match.arg(level)
  has_sf_class <- inherits(.df, "sf")
  
  result <- tibble(dwc_terms = colnames(.df)) |>
    check_contains_terms(y = dwc_terms,
                         is_sf = has_sf_class,
                         level = level)
  .df
}
# NOTE: should probably swap to check_contains() for this.

#' Wait time
#' @noRd
#' @keywords Internal
wait <- function(seconds = 1) {
  Sys.sleep(seconds)
}

#' Theatrics
#' @importFrom cli make_spinner
#' @noRd
#' @keywords Internal
dwc_spinny_message <- function(which) {
  
  # define the spinner
  spinny <- make_spinner(
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
#' @importFrom cli cli_alert
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_alert_danger
#' @importFrom cli cat_line
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
#' @importFrom cli col_grey
#' @importFrom cli symbol
#' @importFrom cli cli_h1
#' @importFrom cli cli_h2
#' @importFrom cli cli_h3
#' @importFrom cli style_italic
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @importFrom stringr str_pad
#' @importFrom tibble lst
#' @noRd
#' @keywords Internal
check_contains_terms <- function(.df, 
                                 y, 
                                 is_sf,
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
  unmatch_message <- c("x" = "Unmatched: {col_red({unmatched_string})}")
  
  all_cols_match <- rlang::is_empty(unmatched_values)
  
  # build message
  all_terms_message <- function(matches_message, 
                                unmatch_message, 
                                all_cols_match) {
    cli_h2("All DwC terms")
    cat_line(cli_text(result_message))
    cli_bullets(matches_message)
    cli_bullets(unmatch_message)
    cli_par()
    if(isTRUE(all_cols_match)) {
      # celebrate
      cat_line(paste0("\n", add_emoji(), " ", col_green("All column names matched DwC terms!"), "\n"))
    }
  }
  
  
  ### Minimum required terms
  
  # retrieve column results
  req_terms_results <- check_required_terms(user_column_names)
  
  # create message components
  req_terms_table <- build_req_terms_table(req_terms_results)
  
  all_req_terms_found <- all(req_terms_results$result == "pass")
  
  # build message
  req_terms_message <- function(table, 
                                all_found) {
    cat_line(table)
    if(isTRUE(all_found)) {
      # celebrate
      cat_line(paste0("\n", add_emoji(), " ", col_green("All minimum requirements met!"), "\n"))
    }
  }
  
  
  ## Suggested workflow
  # browser()
  # Function matching for suggested workflow
  main_functions <- fn_to_term_table()$main
  other_functions <- fn_to_term_table()$optional
  
  suggested_functions <- main_functions |>
    dplyr::filter(!dwc_term %in% matched_values) |>
    dplyr::distinct(use_function) |>
    pull(use_function)
  
  optional_functions <- other_functions |>
    dplyr::filter(dwc_term %in% matched_values) |>
    dplyr::distinct(use_function) |>
    pull(use_function)
  
  # if POINT sf class, suggest `use_coordinates_sf()`
  if(isTRUE(is_sf)) {
      # add
      suggested_functions <- c("use_sf()", suggested_functions)
  }
  
  # add pipe when there are multiple suggested functions
  if(length(suggested_functions) > 1) {
    suggested_functions_piped <- c(paste0(head(suggested_functions, -1), " |> "), tail(suggested_functions, 1))
  } else {
      suggested_functions_piped <- suggested_functions
  }
  
  # add list of optional functions
  if(length(optional_functions) >= 1) {
    optional_functions_string <- ansi_collapse(glue("`{optional_functions}`"),
                                        sep = ", ",
                                        last = ", ",
                                        trunc = 3)
    optional_functions_message <- paste0(
      "{optional_functions_string}") |> cli_text() |> cli_fmt()
  } else {
    optional_functions_message <- NA
  }
  
  workflow_is_empty <- rlang::is_empty(suggested_functions_piped)
  
  # build message
  suggest_message <- function(workflow_is_empty,
                              suggested_functions_piped, 
                              # optional_functions_message, 
                              .envir = parent.frame()) {
    if(!any(workflow_is_empty)) {
      cat_line(style_italic(paste0("\n", "To make your data Darwin Core compliant, use the following workflow:", "\n")))
      cli_text("df |>")
      cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
      lapply(suggested_functions_piped, cli_alert, .envir = .envir)
      cli_end()

    } else {
      cat_line(paste0("\n", add_emoji(), " ", col_green("Your dataframe is Darwin Core compliant!"), "\n"))
      cat_line(paste0("Run checks, or use your dataframe to build a Darwin Core Archive:\n"))
      cli_text("df |>")
      cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
      lapply(paste0("check_dataset()"), cli_alert, .envir = .envir)
      cli_end()
    }
  }
  
  additional_message <- function(optional_functions_message,
                                  .envir = parent.frame()) {
    # cat_line()
    if(!is.na(optional_functions_message)) {
      cli_text(paste0("Based on your matched terms, you can also add to your pipe: ", "\n"))
      cli_bullets(c("*" = optional_functions_message))
    }
    cli_bullets(c("i" = col_grey("See all `use_` functions at {.url https://galaxias.ala.org.au/reference/index.html#add-darin-core-terms}")))
  }
  
  
  
  ### Build final message
  full_alert <- function() {
    
    # DwC terms
    cli_div()
    cli_h1("Darwin Core terms")
    all_terms_message(matches_message, 
                      unmatch_message, 
                      all_cols_match)
    
    cli_h2("Minimum required DwC terms")
    req_terms_message(req_terms_table, 
                      all_req_terms_found)
    cli_end()
    
    # Suggested workflow
    cli_h1("Suggested workflow")
    suggest_message(workflow_is_empty, 
                    suggested_functions_piped, 
                    optional_functions_message)
    
    cli_h3(col_grey("Additional functions"))
    additional_message(optional_functions_message)
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
    ~"use_function", ~"dwc_term",
    "use_occurrences()", "basisOfRecord",
    "use_occurrences()", "occurrenceID",
    "use_scientific_name()", "scientificName",
    "use_coordinates()", "decimalLatitude",
    "use_coordinates()", "decimalLongitude",
    "use_coordinates()", "geodeticDatum",
    "use_coordinates()", "coordinateUncertaintyInMeters",
    "use_datetime()", "eventDate"
  )

  optional <- tibble::tribble(
    ~"use_function", ~"dwc_term",
    "use_locality()", "continent",
    "use_locality()", "country",
    "use_locality()", "countryCode",
    "use_locality()", "stateProvince",
    "use_locality()", "locality",
    "use_taxonomy()", "kingdom",
    "use_taxonomy()", "phylum",
    "use_taxonomy()", "class",
    "use_taxonomy()", "order",
    "use_taxonomy()", "family",
    "use_taxonomy()", "genus",
    "use_taxonomy()", "species",
    "use_taxonomy()", "specificEpithet",
    "use_taxonomy()", "vernacularName",
    "use_abundance()", "individualCount",
    "use_abundance()", "organismQuantity",
    "use_abundance()", "organismQuantityType",
    "use_abundance()", "organismQuantity",
    "use_collection()", "datasetID",
    "use_collection()", "datasetName",
    "use_collection()", "catalogNumber",
    "use_coordinates()", "coordinatePrecision",
    "use_scientific_name()", "scientificNameRank",
    "use_scientific_name()", "scientificNameAuthorship",
    "use_datetime()", "year",
    "use_datetime()", "month",
    "use_datetime()", "day",
    "use_datetime()", "eventTime",
    "use_individual_traits()", "individualID",
    "use_individual_traits()", "lifeStage",
    "use_individual_traits()", "sex",
    "use_individual_traits()", "vitality",
    "use_individual_traits()", "reproductiveCondition",
    "use_observer()", "recordedBy",
    "use_observer()", "recordedByID",
  )

  table <- lst(main, optional) # named list

  return(table)
}




#' Build table for messaging about minimum required terms
#' @importFrom tidyr unnest
#' @importFrom dplyr case_when
#' @importFrom tidyr replace_na 
#' @importFrom cli ansi_align
#' @importFrom cli ansi_nchar
#' @noRd
#' @keywords Internal
build_req_terms_table <- function(req_terms) {
  
  # Unnest & concatenate terms by group
  missing_results <- req_terms |>
    dplyr::select(-matched) |>
    tidyr::unnest(cols = c(missing)) |>
    dplyr::group_by(term_group) |>
    mutate( # glue names
      missing = ansi_collapse(missing, sep = ", ", last = ", ")
    ) |>
    unique()
  
  matched_results <- req_terms |>
    dplyr::select(-missing) |>
    tidyr::unnest(cols = c(matched)) |>
    dplyr::group_by(term_group) |>
    mutate( # glue names
      matched = ansi_collapse(matched, sep = ", ", last = ", ")
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
    ansi_align(col_blue("Type"), max(ansi_nchar(c(pass_group, fail_group)))), " ",
    ansi_align(col_blue("Matched term(s)"), max(ansi_nchar(c(pass_matched, fail_matched)))), " ",
    ansi_align(col_blue("Missing term(s)"), max(ansi_nchar(c(pass_missing, fail_missing)))),"\n",
    collapse = "\n"
  )
  
  bullets_found <- paste0(paste0(
    col_green(symbol$tick), " ", 
    ansi_align(pass_group, max(ansi_nchar(c(pass_group, fail_group)))), " ",
    ansi_align(col_green(pass_matched), max(ansi_nchar(c(pass_matched, fail_matched)))), " ",
    ansi_align(col_red(pass_missing), max(ansi_nchar(c(pass_missing, fail_missing)))), " ",
    collapse = "\n"
  ), "\n")
  
  bullets_missing <- paste0(paste0(
    col_red(symbol$cross), " ",
    ansi_align(fail_group, max(ansi_nchar(c(pass_group, fail_group)))), " ",
    ansi_align(col_green(fail_matched), max(ansi_nchar(c(pass_matched, fail_matched)))), " ",
    ansi_align(col_red(fail_missing), max(ansi_nchar(c(pass_missing, fail_missing)))), " ",
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