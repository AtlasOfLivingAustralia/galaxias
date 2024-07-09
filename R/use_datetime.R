#' Add date and time fields to a `tibble`
#' 
#' This function helps format standard date/time fields to a `tibble`. 
#' 
#' In 
#' practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for how spatial fields are
#' represented in the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param eventDate The date or date + time that the observation/event occurred. 
#' @param year The year of the observation/event.
#' @param month The month of the observation/event.
#' @param day The day of the observation/event. 
#' @param time The time of the observation/event.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Example values are:
#' * `eventDate` should be class `Date` or `POSITct`. We suggest using the 
#' lubridate package to define define your date format using functions like 
#' `ymd()`, `mdy`, `dmy()`, or if including date + time, `ymd_hms()`, 
#' `ymd_hm()`, or `ymd_h()`.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_datetime <- function(
    .df,
    eventDate = NULL,
    year = NULL,
    month = NULL,
    day = NULL,
    time = NULL,
    .keep = "unused"
){
  
  if(missing(.df)){
    abort("df is missing, with no default.")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(day, eventDate, month, time, year)
  names(fn_quos) <- fn_args
  
  # find arguments that are NULL but exist already in `df`
  # then remove their names before `mutate()`
  # otherwise, these DwC columns are deleted by `mutate(.keep = "unused")` 
  fn_quo_is_null <- fn_quos |> 
    purrr::map(\(user_arg)
               rlang::quo_is_null(user_arg)) |> 
    unlist()
  
  null_col_exists_in_df <- fn_quo_is_null & (names(fn_quos) %in% colnames(.df))
  
  if(any(null_col_exists_in_df)){
    fn_quos <- fn_quos |> 
      purrr::keep(!names(fn_quos) %in% names(which(null_col_exists_in_df)))
  }
  
  check_missing_all_args(match.call(), fn_args)
  
  # Update df
  result <- .df |> 
    mutate(!!!fn_quos, 
           .keep = .keep)
  
  check_eventDate(result, level = "abort")
  check_year(result, level = "abort")
  check_month(result, level = "abort")
  check_day(result, level = "abort")
  check_time(result, level = "abort")
    
  # other tests likely to be needed here
  result
}

#' @rdname check_dwc
#' @order 6
#' @importFrom lubridate parse_date_time
#' @export
check_eventDate <- function(.df, 
                            level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "eventDate")){
    .df |>
      select("eventDate") |>
      check_date(level = level) |>
      check_time(level = level)
      # mutate(eventDate = lubridate::parse_date_time(eventDate, orders = "ymd"))
    
    bullets <- c(
      "!" = "{.field eventDate} defaults to UTC standard.",
      i = "To specify a different timezone, use {.code {.pkg lubridate}::ymd_hms(x, tz = \"timezone\")}"
    ) 
    
    cli::cli_inform(bullets)
    
  } 
}

#' @rdname check_dwc
#' @order 6
#' @importFrom lubridate year
#' @importFrom lubridate today
#' @export
check_time <- function(.df, 
                       level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "eventTime")){
    .df |>
      select("eventTime") |>
      check_time(level = level)
  } 
}

#' @rdname check_dwc
#' @order 6
#' @importFrom lubridate year
#' @importFrom lubridate today
#' @export
check_year <- function(.df, 
                       level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "year")){
    .df |>
      select("year") |>
      check_within_range(lower = 0,
                         upper = as.numeric(lubridate::year(lubridate::today())),
                         level = level
                         )
  } 
}

#' @rdname check_dwc
#' @order 6
#' @importFrom lubridate year
#' @importFrom lubridate today
#' @export
check_month <- function(.df, 
                       level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "month")){
    
    month <- .df |>
      select("month")
    
    if(inherits(.df$month, "numeric")) {
      month |>
      check_within_range(lower = 1,
                         upper = 12,
                         level = level
      )
    } else {
      if(inherits(.df$month, "character")) {
        
        # Detect and handle month abbreviations
        if(any(match(.df$month,month.abb))) {
          if(any(is.na(match(.df$month, month.abb)))) {
            cli::cli_warn("Some month abbreviations did not match.")
          }
          } else {
            # Detect and handle month names
            if(any(match(.df$month, month.name))) {
              if(any(is.na(match(.df$month, month.name)))) {
                cli::cli_warn("Some month names did not match.")
              }
              }
          } 
        }
      
    }
  } 
}

#' @rdname check_dwc
#' @order 6
#' @importFrom lubridate year
#' @importFrom lubridate today
#' @export
check_day <- function(.df, 
                        level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "day")){
    
    day <- .df |>
      select("day")
    
    if(inherits(.df$day, "numeric")) {
      day |>
        check_within_range(lower = 1,
                           upper = 31,
                           level = level
        )
    } else {
      bullets <- cli::cli_bullets(c(
        "Invalid {.field day} class.",
        i = "See {.code ?lubridate::mday()} to see how to convert a date to day of month.",
        x = "{.field day} must be numeric, not {class(.df$day)}." 
      ))|>
        cli::cli_fmt()
      
      cli::cli_abort(bullets)
      
    }
  } 
}