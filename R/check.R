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


#' Inform users which columns are being checked
#' 
#' @description
#' Informs users which columns will be checked by `check_` functions. This includes 
#' columns that have been specified in a `use_` function by the user, or columns 
#' that exist in the user dataframe that already match Darwin Core terms.
#' 
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_update
#' @noRd
#' @keywords Internal
col_progress_bar <- function(cols) {
  
  cli::cli_progress_step(
    paste0(
      "Checking {length(cols)} column{?s}: {.field {cols}}"
      ), 
    spinner = TRUE
    )
  
  for (i in 1:length(cols)) {
    Sys.sleep(0.3)
    cli::cli_progress_update()
  }

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
                                 .accepted_message = TRUE,
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
      
      first_line <- c("Unexpected value in {.field {field_name}}.")
      error_line <- c("x" = "Invalid value{?s}: \"{unmatched_string}\"")
      
      if (.accepted_message == TRUE) {
      info_lines <- c(
        i = "Accepted values are {accepted_values}."
        )
      } else {
          info_lines <- NULL
      }
      
      # conditional info message for specific fields
      if(field_name == "countryCode") {
        info_lines <- c(
          i = "{.field {field_name}} accepts two-digit country codes in ISO 3166-1 Alpha 2",
          i = "See {.url https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}"
        )
      }
      
      full_values_message <- c(
        first_line,
        info_lines,
        error_line
        ) |>
        cli::cli_bullets() |>
        cli::cli_fmt()
    }
    
    # withr::with_options(
    #   list(cli.width = 80),
    #   bullets
    # )

    switch_check(level,
                 full_values_message,
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
      "{.field {field_name}} must be a numeric vector, not {class(x)}."
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
    bullets <- c(
      "{.field {field_name}} must be a character vector, not {class(x)}."
    ) |>
      cli_bullets() |>
      cli_fmt()
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
    bullets <- c(
      "Duplicate values in {.field {field_name}}.",
      i = "All values must be unique."
      ) |>
      cli_bullets() |>
      cli_fmt()
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
      "Value is outside of expected range in {.field {field_name}}.",
      i = "Column contains values outside of {lower} <= x <= {upper}."
               )) |> 
        cli::cli_fmt()
    switch_check(level,
                 bullets,
                 call = error_call)
  }
  .df
}

#' check that a field is of a date class
#' @noRd
#' @importFrom lubridate is.timepoint
#' @importFrom lubridate is.POSIXt
#' @importFrom cli cli_fmt
#' @importFrom cli cli_bullets
#' @keywords Internal
check_is_date <- function(.df,
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
    check_is_date_time(x,level)
    }

  .df

}

#' check a vector is date/time format
#' @noRd
#' @importFrom lubridate is.POSIXt
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate ymd_hm
#' @importFrom lubridate ymd_h
#' @importFrom cli cli_fmt
#' @importFrom cli cli_bullets
#' @keywords Internal
check_is_date_time <- function(x,
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

#' check if vector is a valid time format
#' @noRd
#' @importFrom lubridate is.timepoint
#' @importFrom lubridate is.POSIXt
#' @importFrom cli cli_fmt
#' @importFrom cli cli_bullets
#' @keywords Internal
check_is_time <- function(.df,
                          level = "warn",
                          call = caller_env()) {
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)

  # browser()
  # hms::parse_hm(x)

  # time period supplied
  if (any(lubridate::is.period(x)) |
    any(hms::is_hms(x))) {
    x <- x
  } else {
    # character class supplied
    if (any(is.character(x))) {
      # are they time format?
      if (!any(is.na(as.POSIXct(x, format = "%H:%M:%S")))) {
        .df[1] <- hms::parse_hms(x) # hours minutes seconds
      } else {
        if (!any(is.na(as.POSIXct(x, format = "%H:%M")))) {
          .df[1] <- hms::parse_hm(x)
        } else {
          bullets <- c(
            "Invalid time format in {.field {field_name}}.",
            i = "{.field {field_name}} accepts hours:minutes:seconds or hours:minutes."
          ) |>
            cli::cli_bullets() |>
            cli::cli_fmt()

          cli::cli_abort(bullets, call = call)
        }
      }
    } else {
      bullets <- c(
        "Must format {.field {field_name}} as hours:minutes:seconds or hours:minutes.",
        i = "Specify time format with {.pkg lubridate} e.g. {.code hms()} or {.code hm()}}."
      ) |>
        cli::cli_bullets() |>
        cli::cli_fmt()

      cli::cli_abort(bullets, call = call)
    }
  }
  # NOTE: This class isn't retained in final df for some reason
  .df
}

#' check whether all column args are missing in a function call
#' @noRd
#' @keywords Internal
check_missing_all_args <- function(fn_call,
                                   fn_args,
                                   user_cols,
                                   error_call = caller_env()
                                   ){
  # browser()
  function_name <- fn_call[1]
  # user_args <- names(as.list(function_call)[-1])
  
  if (!any(user_cols %in% fn_args)) {
    bullets <- c(
      "No Darwin Core terms detected by {.code {function_name}()}. See {.code ?{function_name}}."
    )
    cli::cli_warn(bullets, call = error_call)
  }
}


#' check whether country code matches country name
#' @noRd
#' @keywords Internal
check_mismatch_code_country <- function(.df, 
                                        level = "inform",
                                        call = caller_env()
                                        ){
  # browser()
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  
  # if(!(.df$countryCode[1] %in% country_codes$country_name)){
  #   bullets <- c("Unrecognised {.field countryCode} value.",
  #                # i = "Did you mean X?",
  #                x = "Did not recognise: {df$countryCode}.")
  #   cli::cli_warn(bullets)
  # }
  
  
  
  lookup_country <- country_codes$country_name[country_codes$code %in% x]
  correct_country <- country_codes$country_name
  if(lookup_country != df$countryCode[1]){
    bullets <- c("Country code in {.field {x}} does not correspond to country.",
                 i = "Did you mean {lookup_country}?"
    )
    cli::cli_warn(bullets)
  }
  .df
}

#' check a vector is a specific length of words
#' @noRd
#' @keywords Internal
check_word_number <- function(.df,
                              max_n_word,
                              level = "inform",
                              call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  
  n_words <- stringr::str_split(x, pattern = " ") |>
    map(\(words)
        length(words)) |>
    unlist()
  
  n_words_too_high <- any(n_words > max_n_word)
  
  if(n_words_too_high){
    bullets <- c(
      "Too many words in each value of {.field {field_name}}.",
      i = "String values must contain a maximum of {max_n_word} word{?s} each."
    ) |>
      cli_bullets() |>
      cli_fmt()
    
    switch_check(level, 
                 bullets,
                 call = call)
  }
  .df
}
