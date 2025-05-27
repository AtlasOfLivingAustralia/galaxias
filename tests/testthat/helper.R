
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
  out |>
    sub("[(][ ]*[.0-9]+ [Mk]B/s[)]", "(8.5 MB/s)", x = _) |>
    sub("[(][.0-9]+/s[)]", "(100/s)", x = _) |>
    sub(" [.0-9]+(ms|s|m)", " 3ms", x = _) |>
    sub("ETA:[ ]*[.0-9]+m?s", "ETA:  1s", x = _) |>
    gsub("\\[[.0-9]+m?s\\]", "[1s]", x = _)
}

fix_filenames <- function(out) {
  out |>
    sub("(?<=file).*?(?=\\.zip)", "12345", x = _, perl = TRUE) |>
    gsub("Path:\\s[[:graph:]]+$", "Path: temp path replaced", x = _, perl = TRUE)
}

fix_emojis <- function(out) {
  sub("(\U0001f600|\U0001f973|\U0001f638|\U0001f308|\U0001f947|\U0001f389|\U0001f38a)", "\U0001f600", out)
  # out <- sub("\U0001f[0-9a-f]{3}", "\U0001f600", out) # doesn't work
}

# this fixes a weird problem whereby slight differences in speed of code
# execution lead to different snapshots
fix_duplicates <- function(out){
  # remove leading and trailing content
  result <- out |>
    gsub("^[[:graph:]]\\s", "", x = _) |>
    sub("\n$", "", x = _)
  # remove whole lines that contain non-useful content
  result <- result[!grepl("\\s[[[:digit:]]+s]", x = result)] # containing [1s] etc
  result <- result[result != ""] # empty entries
  result <- result[!duplicated(result)] # duplicates
  result
}
