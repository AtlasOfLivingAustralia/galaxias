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
    abort("objects supplied to `check_` functions must be a `tibble` or `data.frame`.",
          call = call)
  }
  if(ncol(.df) > 1){
    abort("`data.frame`s supplied to `check_` functions should only have one column.",
          call = call)
  }
  .df
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
  name_lookup <- user_column_names %in% y
  if(any(!name_lookup)){
    
    # Darwin Core term matching
    matched_values <- user_column_names[name_lookup]
    unmatched_values <- user_column_names[!name_lookup]
    matched_string <- ansi_collapse(glue("{matched_values}"),
                                    sep = ", ",
                                    last = " & ")
    unmatched_string <- ansi_collapse(glue("{unmatched_values}"),
                                       sep = ", ",
                                       last = " & ")     
    
    if(length(matched_values) > 0) {
      matches_message <- c(
        "v" = "Matched {length(matched_values)} field name{?s}: {.field {matched_string}}"
      )
    } else {
      matches_message <- NULL
    }
    
    if(length(unmatched_values) > 0) {
      unmatch_message <- c(
        "x" = "No matches for {length(unmatched_values)} field name{?s}: {.field {unmatched_string}}"
      )
    } else {
      unmatch_message <- NULL
    }
    
    bullets <- c(
      matches_message,
      unmatch_message
      )
    
    # browser()
    
    # Function matching for suggested workflow
    dwc_function_main <- tibble::tribble(
      ~"dwc_term", ~"use_function",
      "basisOfRecord",   "use_occurrences()",
      "occurrenceID",   "use_occurrences()",
      "decimalLatitude",   "use_coordinates()",
      "decimalLongitude",   "use_coordinates()",
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
      
      # DwC matches
      cli::cli_div()
      cli::cli_h1("DwC terms")
      cli::cli_bullets(bullets)
      cli::cli_end()
      
      # Suggested workflow
      cli::cli_h1("Suggested workflow")
      cli::cli_text("To make your data Darwin Core compliant, use the following workflow:")
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
  }
  .df
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
      "{.field {field_name}} is not numeric."
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
    bullets <- c(i = glue("`{field_name}` contains values ouside of {lower} <= x <= {upper}"))
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
      "Invalid {.field eventDate} class.",
      i = "Use {.pkg lubridate} to work with date/time data.",
      i = "Specify date format with e.g. {.code ymd()}, {.code mdy()}, or {.code dmy()}.",
      x = "{.field eventDate} must be a Date vector, not a {class(x)}."
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
        "{.field eventDate} contains invalid date time format.",
        i = "Use {.pkg lubridate} to work with date/time data.",
        i = "Specify date/time format with e.g. {.code ymd_hms()}, {.code ymd_hm()}, or {.code ymd_h()}."
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
      cli::cli_abort("Only accepts hours minutes or hours minutes seconds.")
    }
  }

  .df
}