.onLoad <- function(libname, pkgname) {
  .GlobalEnv$log_env <- initialize_log_env()
}

initialize_log_env <- function() {
  log_env <- new.env()
  log_env$logs <- list(
    validation = list(),
    user_actions = list(),
    general = list()
  )
  return(log_env)
}

log_message <- function(message,
                        category = "general",
                        name = NULL,
                        print = TRUE,
                        type = "info") {
  if (!exists("logs", envir = log_env)) {
    stop("Logging environment is not initialised.")
  }
  if (!category %in% names(log_env$logs)) {
    stop("Invalid log category")
  }
  if (is.null(name)) {
    name <- paste(category, Sys.time(), sep = "_")
  }
  log_env$logs[[category]][[name]] <- paste(message, Sys.time(), sep = " | ")
  if (print) {
    switch(type,
      success = cli::cli_alert_success(message),
      error = cli::cli_alert_danger(message),
      warning = cli::cli_alert_warning(message),
      info = cli::cli_alert_info(message),
      plain = cli::cli_alert(message),
      cli::cli_alert(message)
    )
  }
}

get_log_messages <- function(category, name = NULL) {
  if (!exists("logs", envir = log_env)) {
    stop("Logging environment is not initialised.")
  }
  if (!category %in% names(log_env$logs)) {
    stop("Invalid log category")
  }
  if (is.null(name)) {
    return(log_env$logs[[category]])
  } else {
    if (!name %in% names(log_env$logs[[category]])) {
      stop("Log name not found in the category")
    }
    return(log_env$logs[[category]][[name]])
  }
}

default_log_path <- "default_logs.RData"
# Function to get the current log path
get_log_path <- function() {
  return(Sys.getenv("MY_PACKAGE_LOG_PATH", default_log_path))
}

# --- Alternative method ---

# user set custom log path
# set_log_path <- function(new_path) {
#   Sys.setenv(MY_PACKAGE_LOG_PATH = new_path)
# }
# save_logs <- function(logs) {
#   save(logs, file = get_log_path())
# }
# load_logs <- function() {
#   log_path <- get_log_path()
#   if (file.exists(log_path)) {
#     load(log_path)
#     return(logs)
#   } else {
#     return(create_new_log())
#   }
# }
# add_log_entry <- function(logs, category, message, name) {
#   logs[[category]][[name]] <- message
#   save_logs(logs)
# }
# get_specific_log <- function(category, name) {
#   logs <- load_logs()
#   if (is.null(logs[[category]][[name]])) {
#     stop("Log entry not found")
#   }
#   return(logs[[category]][[name]])
# }
# in generate report:
# logs <- load_logs()
# validation_log <- if ("validation" %in% names(logs)) logs$validation else list()
