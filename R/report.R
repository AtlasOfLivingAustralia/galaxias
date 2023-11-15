#' Report generation
#'
#' Understanding the logging and report behaviours and edge cases:
#' The idea of the log environment is that a user can run individual functions
#' within a session, and then decide to print a report at the end of their
#' session. As the list is in an env, we can access it anywhere. The downside is
#' it can be a little more difficult to manage and debug if issues crop up. The
#' alternative approach is we only allow a report to be generated at
#' the end of complete pipeline run. The advantage of this E2E approach would be
#' easier handling of logs - just passing a named list through the pipeline
#' appending logs, which would probably be more reliable and easier to manage.
#'
#' Notes on log env behaviours (in progress):
#' * entries are overwritten
#'   * run validate latitude twice, the first log entry will be overwritten
#' * Using `load_all()` clears the log_env because of `.onLoad()`
#' * Custom output directory possible but requires extra steps
#' @param data data frame to be used in the report
generate_report <- function(data) {
  template_path <- system.file("markdown/report_template.qmd",
    package = "galaxias"
  )
  # Compile logs if needed
  # This collapses the log messages into a single string w/ newlines
  user_actions_log <- paste(
    get_log_messages("user_actions"),
    collapse = "\n"
  )
  general_log <- paste(
    get_log_messages("general"),
    collapse = "\n"
  )

  # Just passing validation log for now
  # Data frame can't be passed to quarto render unless serialised first
  quarto::quarto_render(
    input = template_path,
    execute_params = list(
      data = jsonlite::toJSON(data),
      validation_log = log_env$logs$validation
    ),
    execute = TRUE,
    cache_refresh = TRUE
  )
}
