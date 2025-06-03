#' @rdname galaxias_config
#' @order 2
#' @param x An object of class `galaxias_config`, created with 
#' `galaxias_config()`.
#' @param \dots Additional arguments, currently ignored.
#' @export
print.galaxias_config <- function(x, ...){
  cli::cli_h3("{.pkg galaxias} configuration")
  cli::cli_text("{.strong File configuration}")
  if(!is.null(x$directory)){
    cli::cli_bullets(c("*" = "directory: {x$directory}")) 
  }
  if(!is.null(x$archive)){
    cli::cli_bullets(c("*" = "archive: {x$archive}"))
    cli::cli_bullets(c(" " = cli::col_green("path: {fs::path_abs(glue::glue('../{x$archive}'))}")))
  }
  if(!is.null(x$gbif)){
    cli::cli_text("{.strong GBIF API credentials}")
    api_config <- purrr::map(seq_len(length(x$gbif)),
                              .f = \(a){
                                glue::glue("{names(x$gbif)[a]}: {x$gbif[[a]]}")
                              }) |>
      unlist()
    names(api_config) <- rep("*", length(x$gbif))
    cli::cli_bullets(api_config)
  }
}