#' Validate
#' Validation checks on data. Logs are saved.
#' Todo: store <- list() store true/false or other results from each
#' validation for a final report output
#' @param data A data frame
#' @return Console output
#' @export
validate <- function(data) {
  validate_decimal_latitude(data, store)
  validate_decimal_longitude(data)
  validate_event_date(data)
}

#' Validate latitude
#' Check that values are within the valid range of -90 to 90
#' @param data data frame
#' @return A message to the console
#' @rdname validate
#' @keywords internal
validate_decimal_latitude <- function(data) {
  if ("decimalLatitude" %in% colnames(data)) {
    if (all(data$decimalLatitude >= -90, na.rm = TRUE) &&
      all(data$decimalLatitude <= 90, na.rm = TRUE)) {
      log_message(
        message = "Pass: All 'decimalLatitude' values are within valid range",
        category = "validation",
        name = "decimal_latitude",
        type = "success"
      )
      return(invisible(TRUE))
    } else {
      log_message(
        message = paste0(
          "Fail: Some 'decimalLatitude' values are outside the",
          " range -90 to 90."
        ),
        category = "validation",
        name = "decimal_latitude",
        type = "error"
      )
      return(invisible(FALSE))
    }
  } else {
    # No log needed, better to log a complete column check all together
    cli::cli_alert_warning("Warning: 'decimalLatitude' column not found.")
    return(invisible("warning"))
  }
}

#' Validate longitude
#' Check that values are within the valid range of -180 to 180
#' @param data A data frame
#' @return A message to the console
#' @rdname validate
#' @keywords internal
validate_decimal_longitude <- function(data) {
  if ("decimalLongitude" %in% colnames(data)) {
    if (all(data$decimalLongitude >= -180, na.rm = TRUE) &&
      all(data$decimalLongitude <= 180, na.rm = TRUE)) {
      log_message(
        message = "Pass: All 'decimalLongitude' values are within valid range",
        category = "validation",
        name = "decimal_longitude",
        type = "success"
      )
      return(invisible(TRUE))
    } else {
      log_message(
        message = paste0(
          "Fail: Some 'decimalLongitude' values are outside",
          "the range -180 to 180."
        ),
        category = "validation",
        name = "decimal_longitude",
        type = "error"
      )
      return(invisible(FALSE))
    }
  } else {
    cli::cli_alert_warning("Warning: 'decimalLongitude' column not found.")
    return(invisible(FALSE))
  }
}

#' Validate eventDate
#' * validate against accepted regex patterns
#' * validate year, month, and day if present
#' * see term details [here](https://dwc.tdwg.org/terms/#dwc:eventDate)
#' @param data data frame
#' @return A message to the console
#' @rdname validate
#' @keywords internal
validate_event_date <- function(data) {
  if ("eventDate" %in% colnames(data)) {
    data$eventDate <- as.character(data$eventDate)

    # regex sourced from https://github.com/gbif/gbif-data-validator/blob/
    # 6e2162e57c104ad9e4f3b7d5c76587a1bf942707/validator-processor/src/main/
    # resources/xml/dwc/tdwg_basetypes.xsd#L76 # nolint

    pattern <- "\\d{4}(-(0[1-9]|1[012])(-((0[1-9])|1\\d|2\\d|3[01])(T(0\\d|1\\d|2[0-3])(:[0-5]\\d){0,2})?)?)?|\\-\\-(0[1-9]|1[012])(-(0[1-9]|1\\d|2\\d|3[01]))?|\\-\\-\\-(0[1-9]|1\\d|2\\d|3[01])" # nolint
    valid_format <- sapply(data$eventDate, function(date) {
      grepl(pattern, date)
    })

    valid_ymd <- sapply(data[valid_format, ]$eventDate, function(date) {
      validate_month_day(date)
    })
    if (all(valid_format) && all(valid_ymd)) {
      log_message(
        message = "Pass: All 'eventDate' values are in a valid date format",
        category = "validation",
        name = "event_date",
        type = "success"
      )
      return(invisible(TRUE))
    } else {
      log_message(
        message = paste(
          "Detected invalid 'eventDate' values"
        ),
        category = "validation",
        name = "event_date",
        type = "error"
      )
      print(data$eventDate[!valid_format])
      # different handling than above because it is indexed against a subset of
      # data not the original data
      print(names(valid_ymd)[which(valid_ymd == FALSE)])
    }
  } else {
    cli::cli_alert_warning("Warning: 'eventDate' column not found.")
  }
}

#' Validate eventDate
#' * validate against some accepted patterns (gbif)
#' * detect yyyy-mm-dd formats and validate them
#' @param data data frame
#' @return A message to the console
#' @rdname validate
#' @keywords internal
validate_month_day <- function(date) {
  # Extract year, month, and day using regex
  matches <- regmatches(date, regexec("(\\d{4})-(\\d{2})-(\\d{2})", date))
  # browser()
  if (length(matches[[1]]) < 4) {
    return(TRUE)
  } else {
    year <- as.integer(matches[[1]][2])
    month <- as.integer(matches[[1]][3])
    day <- as.integer(matches[[1]][4])

    # Check if month and day are valid
    return(month %in% 1:12 &&
      day %in% 1:31 &&
      !is.na(ISOdate(year, month, day)))
  }
}
