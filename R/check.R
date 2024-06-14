#' Internal function to report at the requested severity level
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom rlang abort
#' @importFrom rlang caller_env
#' @importFrom cli cli_inform
#' @importFrom cli cli_warn
#' @importFrom cli cli_abort
#' @noRd
#' @keywords Internal
switch_check <- function(level = "inform", 
                         bullets = "", 
                         call = caller_env()){
  switch(level, 
         "inform" = cli_inform(bullets, call = call),
         "warn" = cli_warn(bullets, call = call),
         "abort" = cli_abort(bullets, call = call))
}

#' Internal function used to catch errors in low-level `check_` functions
#' As these are only ever called internally, this is basically a debugging assistant.
#' @importFrom rlang abort
#' @noRd
#' @keywords internal
check_data_frame <- function(.df,
                             call = caller_env()
){
  if(!inherits(.df, "data.frame")){
    abort("Must supply a `tibble` or `data.frame` to `check_` functions.",
          call = call)
  }
  if(ncol(.df) > 1){
    abort("Must supply `data.frame` with one column to `check_` functions.",
          call = call)
  }
  .df
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
  # if(any(!name_lookup)){
    
    ## All terms
    # matches
    matched_values <- user_column_names[name_lookup]
    unmatched_values <- user_column_names[!name_lookup]
    
    all_terms_message <- all_terms_message(user_column_names,
                                           matched_values,
                                           unmatched_values)
    
    ## Minimum required terms
    
    req_terms_results <- check_required_terms(user_column_names)
    
    req_terms_message <- req_terms_message(req_terms_results)
    
    
    ## Suggested workflow
    
    # Function matching for suggested workflow
    dwc_function_main <- tibble::tribble(
      ~"dwc_term", ~"use_function",
      "basisOfRecord",   "use_occurrences()",
      "occurrenceID",   "use_occurrences()",
      "decimalLatitude",   "use_coordinates()",
      "decimalLongitude",   "use_coordinates()",
      "geodeticDatum",   "use_coordinates()",
      "coordinateUncertaintyInMeters",   "use_coordinates()",
      "eventDate",   "use_datetime()"
    )
    
    dwc_function_optional <- tibble::tribble(
      ~"dwc_term", ~"use_function",
      "continent",   "use_locality",
      "country",   "use_locality",
      "countryCode",   "use_locality",
      "stateProvince",   "use_locality",
      "locality",   "use_locality"
    )
    
    suggested_functions <- dwc_function_main |>
      dplyr::filter(!dwc_term %in% matched_values) |>
      dplyr::distinct(use_function) |>
      pull(use_function)
    
    optional_functions <- dwc_function_optional |>
      dplyr::filter(!dwc_term %in% matched_values) |>
      dplyr::distinct(use_function) |>
      pull(use_function)
    
    if(length(suggested_functions) > 1) {
      suggested_functions_piped <- c(paste0(head(suggested_functions, -1), " |> "), tail(suggested_functions, 1))
    } else {
        suggested_functions_piped <- suggested_functions
      }

    
    if(length(optional_functions) >= 1) {
      optional_functions <- ansi_collapse(glue("{optional_functions}"),
                                          sep = ", ",
                                          last = ", ")
    }
    
    # Format message
    custom_alert <- function(texts, other_texts, .envir = parent.frame()) {
      
      # DwC terms
      cli::cli_div()
      cli::cli_h1("DwC terms")
      cli::cli_h2("All terms")
      cli::cat_line(all_terms_message)
      cli::cli_h2("Minimum required terms")
      cli::cat_line(req_terms_message)
      cli::cli_end()
      
      # Suggested workflow
      cli::cli_h1("Suggested workflow")
      cli::cat_line(paste0("\n", "To make your data Darwin Core compliant, use the following workflow:"))
      cli::cli_par()
      cli::cli_end()
      cli::cli_text("df |>")
      cli::cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
      lapply(texts, cli::cli_alert, .envir = .envir)
      cli::cli_par()
      cli::cli_end()
      cli::cli_div()
      cli::cli_text(cli::col_grey("Additional functions: {.fn {other_texts}}"))
      cli::cli_end()
      cli::cli_par()
    }
    
    # withr::with_options(
    #   list(cli.width = 80),
    #   custom_alert(suggested_functions_piped, optional_functions)
    # )
    
    custom_alert(suggested_functions_piped, optional_functions)
    
    # cli_inform(fun(), call = call)
    
    
    # switch_check(level, 
    #              bullets,
    #              call = call)
  # }
  .df
}


#' Build message about all matched DwC terms
#' @importFrom cli ansi_collapse
#' @importFrom cli cli_bullets
#' @importFrom cli cli_fmt
#' @noRd
#' @keywords Internal
all_terms_message <- function(user_column_names,
                              matched_values,
                              unmatched_values) {
  
  # concatenate matched/unmatched fields
  matched_string <- ansi_collapse(glue("{matched_values}"),
                                  sep = ", ",
                                  last = ", ")
  unmatched_string <- ansi_collapse(glue("{unmatched_values}"),
                                    sep = ", ",
                                    last = ", ")
  
  if(length(matched_values) > 0) {
    matches_message <- c(
      "v" = "Matched {length(matched_values)} of {length(user_column_names)} column name{?s} to DwC terms: {.field {matched_string}}"
    )
  } else {
    matches_message <- NULL
  }
  
  if(length(unmatched_values) > 0) {
    unmatch_message <- c(
      "x" = "Unmatched column name(s): {cli::col_red({unmatched_string})}"
    )
    
  } else {
    # celebrate
    unmatch_message <- paste0("\n", add_emoji(), " ", cli::col_green("All column names matched DwC terms!"))
  }
  
  # build message
  c(
    matches_message,
    unmatch_message,
    "\n"
  ) |> 
    cli::cli_bullets() |> 
    cli::cli_fmt() # save cli formatted message
  
}

#' Build message about minimum required terms
#' @noRd
#' @keywords Internal
req_terms_message <- function(req_terms) {
  
  # Unnest & concatenate terms by group
  req_terms_message <- req_terms |>
    tidyr::unnest(cols = c(missing, matched)) |>
    dplyr::group_by(term_group) |> 
    mutate(
      missing = cli::ansi_collapse(missing, sep = ", ", last = ", "),
      matched = cli::ansi_collapse(matched, sep = ", ", last = ", ")
    ) |>
    unique() |>
    # add blank space for correct message formatting
    tidyr::replace_na(list(missing = stringr::str_pad("-", width = 16, side = "right"),
                           matched = stringr::str_pad("-", width = 16, side = "right")))
    
  browser()

  # Group terms found vs missing
  found <- req_terms_message |>
    dplyr::filter(result == "pass")
  
  missing <- req_terms_message |>
    dplyr::filter(result == "fail")
  
  pass_group <- glue("{found$term_group}")
  pass_matched <- glue("{found$matched}")
  pass_missing <- glue("{found$missing}")
  fail_group <- glue("{missing$term_group}")
  fail_matched <- glue("{missing$matched}")
  fail_missing <- glue("{missing$missing}")
  
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
  if(nrow(found) == 0) {
    bullets_found <- NULL
  }
  
  if(nrow(missing) == 0) {
    # celebrate
    bullets_missing <- paste0("\n", add_emoji(), " ", cli::col_green("All minimum requirements met!"))
  }
  
  # final message
  paste0(
    headers, 
    bullets_found, 
    bullets_missing
  ) 
}

#' check a vector consists only of values in a second vector
#' @param x vector of values
#' @param y vector against which x should be compared
#' @importFrom dplyr pull
#' @importFrom glue glue
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_alert_danger
#' @importFrom cli cli_text
#' @importFrom cli cli_bullets
#' @importFrom cli ansi_collapse
#' @noRd
#' @keywords Internal
check_contains_values <- function(.df, 
                                 y, 
                                 level = "inform",
                                 call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  user_values <- .df |> 
    pull(field_name) |>
    unique() |>
    sort()
  name_lookup <- user_values %in% y
  if(any(!name_lookup)){
    unmatched_values <- user_values[!name_lookup]
    unmatched_string <- ansi_collapse(glue("{unmatched_values}"),
                                      sep = ", ",
                                      last = " & ")     
    
    if(length(unmatched_values) > 0) {
      accepted_values <- ansi_collapse(glue("\"{y}\""),
                                       sep = ", " ,
                                       last = " & ")
      unmatch_message <- c(
        "Unexpected value in {.field {field_name}}.",
        i = "Accepted values are {accepted_values}.",
        "x" = "Unexpected value{?s}: \"{unmatched_string}\""
      )
      bullets <- cli::cli_bullets(c(
        unmatch_message
      )) |>
        cli::cli_fmt()
    }
    
    # withr::with_options(
    #   list(cli.width = 80),
    #   bullets
    # )

    switch_check(level,
                bullets,
                call = call)
  }
  .df
}

#' check a vector is numeric
#' @noRd
#' @keywords Internal
check_is_numeric <- function(.df, 
                             level = "inform",
                             call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  if(!inherits(x, c("numeric", "integer"))){
    
    bullets <- cli::cli_bullets(c(
      "{.field {field_name}} must be numeric vector, not {class(x)}."
      )) |>
      cli::cli_fmt()
    
    switch_check(level,
                 bullets,
                 call = call)
  }
  .df
}

#' check a vector is a string
#' @noRd
#' @keywords Internal
check_is_string <- function(.df,
                            level = "inform",
                            call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  if(!inherits(x, "character")){
    bullets <- c(i = glue("`{field_name}` is not a string"))
    switch_check(level, 
                 bullets,
                 call = call)
  }
  .df
}

#' check a vector has one row per value
#' @noRd
#' @keywords Internal
check_unique <- function(.df,
                         level = "inform",
                         call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  unique_check <- length(unique(x)) == length(x)
  if(!unique_check){
    bullets <- c(i = glue("`{field_name}` does not contain a unique value in each cell"))
    switch_check(level, 
                 bullets,
                 call = call)
  }
  .df
}

#' check a vector is within a specified range
#' @noRd
#' @keywords Internal
check_within_range <- function(.df, 
                               level = "inform", 
                               lower,
                               upper,
                               error_call = caller_env()
){
  .df |> 
    check_data_frame() |>
    check_is_numeric(level = level, call = caller_env())
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  range_check <- (x >= lower & x <= upper)
  if(!all(range_check)){
    bullets <- cli::cli_bullets(c(
      "Value outside of expected range.",
      x = "`{field_name}` contains values ouside of {lower} <= x <= {upper}."
               )) |> 
        cli::cli_fmt()
    switch_check(level,
                 bullets,
                 call = error_call)
  }
  .df
}

#' check a vector has one row per value
#' @noRd
#' @importFrom lubridate is.timepoint
#' @importFrom lubridate is.POSIXt
#' @importFrom cli cli_fmt
#' @importFrom cli cli_bullets
#' @keywords Internal
check_date <- function(.df,
                       level = "warn",
                       call = caller_env()
                       ){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  
  # Is it a date?
  if(!lubridate::is.timepoint(x)){
    bullets <- cli::cli_bullets(c(
      "{.field eventDate} must be a Date vector, not a {class(x)}.",
      i = "Specify date format with {.pkg lubridate} functions e.g. {.code ymd()}, {.code mdy()}, or {.code dmy()}."
      )) |>
      cli::cli_fmt()
    
    switch_check(level,
                 bullets,
                 call = call)
  }
  
  if(any(lubridate::is.POSIXt(x))) {
    check_date_time(x,level)
    }

  .df

}

#' check a vector has one row per value
#' @noRd
#' @importFrom lubridate is.POSIXt
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate ymd_hm
#' @importFrom lubridate ymd_h
#' @importFrom cli cli_fmt
#' @importFrom cli cli_bullets
#' @keywords Internal
check_date_time <- function(x,
                            level = "warn",
                            call = caller_env()
){

  
  # browser()
  # Is there also a time?
  if(any(lubridate::is.POSIXt(x))) {
    
    # Is the time formatted as ymd_hms, ymd_hm or ymd_h?
    if(!any(is.na(lubridate::ymd_hms(x, quiet = TRUE))) |
       !any(is.na(lubridate::ymd_hm(x, quiet = TRUE))) |
       !any(is.na(lubridate::ymd_h(x, quiet = TRUE)))
    ) {
      x <- x
    } else {
      bullets <- cli::cli_bullets(c(
        "{.field eventDate} contains invalid date/time format.",
        i = "Specify date/time format with {.pkg lubridate} functions e.g. {.code ymd_hms()}, {.code ymd_hm()}, or {.code ymd_h()}."
      )) |>
        cli::cli_fmt()
      
      cli::cli_abort(bullets, call = call)
    }
  }
  
  x
  
}

#' check a vector has one row per value
#' @noRd
#' @importFrom lubridate is.timepoint
#' @importFrom lubridate is.POSIXt
#' @importFrom cli cli_fmt
#' @importFrom cli cli_bullets
#' @keywords Internal
check_time <- function(.df,
                       level = "warn",
                       call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  
  # Is there a time?
  if(any(lubridate::is.POSIXt(x))) {
    
    # what format is the time?
    if(!any(is.na(as.POSIXct(x, format = "%H:%M:%S"))) |
       !any(is.na(as.POSIXct(x, format = "%H:%M")))) { 

      if(any(is.na(as.POSIXct(x, format = "%H:%M")))) { 
        .df[1] <- hms::parse_hms(.df[1], format = "%H:%M") # hours minutes seconds
    } else {
      .df[1] <- hms::parse_hm(.df[1]) # hours minutes
    }
    } else {
      cli::cli_abort("Must format {.field {field_name}} as hours:minutes or hours:minutes:seconds.")
    }
  }

  .df
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
#' @improtFrom dplyr bind_rows
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