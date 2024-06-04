#' Add date and time fields to a `tibble`
#' 
#' This function helps format standard date/time fields to a `tibble`. 
#' 
#' In 
#' practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for how spatial fields are
#' represented in the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param eventDate The date and/or time that the observation/event occurred. 
#' @param year The year of the observation/event.
#' @param month The month of the observation/event.
#' @param day The day of the observation/event. 
#' that contains the whole location, given any possible measurement error.
#' @param eventTime The time of the observation/event.
#' @param datePrecision Precision of the `eventDate`.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Example values are:
#' * `geodeticDatum` should be a valid EPSG code
#' * `coordinatePrecision` should be no less than 0.00001 if data were collected
#' using GPS
#' * `coordinateUncertaintyInMeters` will typically be around `30` (metres) if
#' recorded with a GPS after 2000, or `100` before that year. 
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_datetime <- function(
    df,
    eventDate = NULL,
    year = NULL,
    month = NULL,
    day = NULL,
    eventTime = NULL,
    eventDatePrecision = NULL,
    .keep = "unused"
){
  
  if(missing(df)){
    abort("df is missing, with no default.")
  }
  
  # Handle eventDate
  
  ## TODO: Allow df |> use_datetime(eventDate = c(date, time))
  
  # error if more than 2 columns supplied
  # if(length(eventDate) > 2) {
  #   cli::cli_abort("Too many inputs supplied to {.field eventDate}.")
  # } else {
  # 
  #   # 
  #   if(length(eventDate) == 2) {
  #     
  #     temp_df <- df |>
  #       dplyr::select({{eventDate}}[1], {{eventDate}}[[2]])
  #     
  #     check_eventDate(temp_df[1], level = "abort")
  #     check_eventTime(temp_df[2], level = "abort")
  #     
  #     eventDate <- lubridate::date(glue::glue("{temp_df$date} {temp_df$time}"))
  #   } 
  # }
  
  result <- df |>
    mutate(eventDate = {{eventDate}},
           eventTime = {{eventTime}},
           year = {{year}},
           month = {{month}},
           day = {{day}},
           eventDatePrecision = {{eventDatePrecision}},
           .keep = .keep)
  
  check_eventDate(result, level = "abort")
  check_eventTime(df, level = "abort")
  check_year(result, level = "abort")
  # check_month(df, level = "abort")
  # check_day(df, level = "abort")

  # check_DatePrecision(df, level = "abort")
    
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
      mutate(eventDate = lubridate::parse_date_time(eventDate, orders = "ymd"))
    
    bullets <- c(
      "!" = "{.field eventDate} defaults to UTC standard.",
      i = "To specify a different timezone, use {.code {.pkg lubridate}::ymd_hms(x, tz = \"timezone\")}"
    ) 
    
    cli::cli_warn(bullets)
    
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
check_eventTime <- function(.df, 
                       level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "eventTime")){
    .df |>
      select("eventTime") |>
      check_time(level = level)
  } 
}