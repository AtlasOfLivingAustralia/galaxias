# validate_decimal_latitude gives warning if no decimalLatitude
column is found

    Code
      local({
        validate_decimal_latitude(data.frame(latitude = -95.1234))
      })
    Message <cliMessage>
      ! Warning: 'decimalLatitude' column not found.

