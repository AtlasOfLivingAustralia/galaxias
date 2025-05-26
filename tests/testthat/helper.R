
capture_cli_messages <- function(expr) {
  msgs <- character()
  withCallingHandlers(
    expr,
    cliMessage = function(e) {
      msgs <<- c(msgs, conditionMessage(e))
      invokeRestart("muffleMessage")
    }
  )
  msgs
}

capture_cli_warnings <- function(expr) {
  wrns <- character()
  withCallingHandlers(
    expr,
    cliWarning = function(w) {
      wrns <<- c(wrns, warningCondition(w))
      rlang::cnd_muffle()
    }
  )
  wrns
}

fix_times <- function(out) {
  out <- sub("[(][ ]*[.0-9]+ [Mk]B/s[)]", "(8.5 MB/s)", out)
  out <- sub("[(][.0-9]+/s[)]", "(100/s)", out)
  out <- sub(" [.0-9]+(ms|s|m)", " 3ms", out)
  out <- sub("ETA:[ ]*[.0-9]+m?s", "ETA:  1s", out)
  out <- gsub("\\[[.0-9]+m?s\\]", "[1s]", out)
  out
}

fix_filenames <- function(out) {
  out <- sub("(?<=file).*?(?=\\.zip)", "12345", out, perl = TRUE)
  out
}

fix_emojis <- function(out) {
  out <- sub("(\U0001f600|\U0001f973|\U0001f638|\U0001f308|\U0001f947|\U0001f389|\U0001f38a)", "\U0001f600", out)
  # out <- sub("\U0001f[0-9a-f]{3}", "\U0001f600", out) # doesn't work
  out
}