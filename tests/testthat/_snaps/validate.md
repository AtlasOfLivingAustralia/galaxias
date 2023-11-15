# validate wrapper function works (fails quietly) if some inputs are valid and
  some aren't [plain]

    Code
      local({
        validate(df)
      })
    Message <cliMessage>
      v Pass: All 'decimalLatitude' values are within valid range
      ! Warning: 'decimalLongitude' column not found.
      v Pass: All 'eventDate' values are in a valid date format

# validate wrapper function works (fails quietly) if some inputs are valid and
  some aren't [ansi]

    Code
      local({
        validate(df)
      })
    Message <cliMessage>
      [32mv[39m Pass: All 'decimalLatitude' values are within valid range
      [33m![39m Warning: 'decimalLongitude' column not found.
      [32mv[39m Pass: All 'eventDate' values are in a valid date format

# validate wrapper function works (fails quietly) if some inputs are valid and
  some aren't [unicode]

    Code
      local({
        validate(df)
      })
    Message <cliMessage>
      ✔ Pass: All 'decimalLatitude' values are within valid range
      ! Warning: 'decimalLongitude' column not found.
      ✔ Pass: All 'eventDate' values are in a valid date format

# validate wrapper function works (fails quietly) if some inputs are valid and
  some aren't [fancy]

    Code
      local({
        validate(df)
      })
    Message <cliMessage>
      [32m✔[39m Pass: All 'decimalLatitude' values are within valid range
      [33m![39m Warning: 'decimalLongitude' column not found.
      [32m✔[39m Pass: All 'eventDate' values are in a valid date format

# CLI warning message if no `decimalLatitude` column detected [plain]

    Code
      local({
        validate_decimal_latitude(data.frame(latitude = 50))
      })
    Message <cliMessage>
      ! Warning: 'decimalLatitude' column not found.

# CLI warning message if no `decimalLatitude` column detected [ansi]

    Code
      local({
        validate_decimal_latitude(data.frame(latitude = 50))
      })
    Message <cliMessage>
      [33m![39m Warning: 'decimalLatitude' column not found.

# CLI warning message if no `decimalLatitude` column detected [unicode]

    Code
      local({
        validate_decimal_latitude(data.frame(latitude = 50))
      })
    Message <cliMessage>
      ! Warning: 'decimalLatitude' column not found.

# CLI warning message if no `decimalLatitude` column detected [fancy]

    Code
      local({
        validate_decimal_latitude(data.frame(latitude = 50))
      })
    Message <cliMessage>
      [33m![39m Warning: 'decimalLatitude' column not found.

# CLI warning message if no `decimalLongitude` column detected [plain]

    Code
      local({
        validate_decimal_longitude(data.frame(Longitude = 20))
      })
    Message <cliMessage>
      ! Warning: 'decimalLongitude' column not found.

# CLI warning message if no `decimalLongitude` column detected [ansi]

    Code
      local({
        validate_decimal_longitude(data.frame(Longitude = 20))
      })
    Message <cliMessage>
      [33m![39m Warning: 'decimalLongitude' column not found.

# CLI warning message if no `decimalLongitude` column detected [unicode]

    Code
      local({
        validate_decimal_longitude(data.frame(Longitude = 20))
      })
    Message <cliMessage>
      ! Warning: 'decimalLongitude' column not found.

# CLI warning message if no `decimalLongitude` column detected [fancy]

    Code
      local({
        validate_decimal_longitude(data.frame(Longitude = 20))
      })
    Message <cliMessage>
      [33m![39m Warning: 'decimalLongitude' column not found.

# CLI warning message if no `eventDate` column detected [plain]

    Code
      local({
        validate_event_date(data.frame(date = "hello"))
      })
    Message <cliMessage>
      ! Warning: 'eventDate' column not found.

# CLI warning message if no `eventDate` column detected [ansi]

    Code
      local({
        validate_event_date(data.frame(date = "hello"))
      })
    Message <cliMessage>
      [33m![39m Warning: 'eventDate' column not found.

# CLI warning message if no `eventDate` column detected [unicode]

    Code
      local({
        validate_event_date(data.frame(date = "hello"))
      })
    Message <cliMessage>
      ! Warning: 'eventDate' column not found.

# CLI warning message if no `eventDate` column detected [fancy]

    Code
      local({
        validate_event_date(data.frame(date = "hello"))
      })
    Message <cliMessage>
      [33m![39m Warning: 'eventDate' column not found.

