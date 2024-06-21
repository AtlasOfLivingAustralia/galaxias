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

#' check whether all column args are missing in a function call
#' @noRd
#' @keywords Internal
check_missing_args <- function(function_call,
                               args,
                               error_call = caller_env()
                               ){
  function_name <- function_call[1]
  function_args <- args
  user_args <- names(as.list(function_call)[-1])
  
  if (length(user_args) == 1 && user_args %in% "df") {
    bullets <- c(
      "No arguments supplied to {.code {function_name}()}.",
      i = "See {.code ?{function_name}} for valid arguments."
    )
    cli::cli_abort(bullets, call = caller_env())
  }
  
}
