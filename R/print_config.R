#' @rdname galaxias_config
#' @order 2
#' @param x An object of class `galaxias_config`, created with 
#' `galaxias_config()`.
#' @param \dots Additional arguments, currently ignored.
#' @export
print.galaxias_config <- function(x, ...){
  cli::cli_h3("{.pkg galaxias} configuration")
  
  cli::cli_text("{.strong File configuration}")
  
  file_config <- purrr::map(c(1, 2),
                            .f = \(a){
                              glue::glue("{names(x)[a]}: {x[[a]]}")
                            }) |>
    unlist()
  names(file_config) <- rep("*", 2)
  cli::cli_bullets(file_config)
  
  if(!is.null(x$gbif)){
    cli::cli_text("{.strong GBIF API credentials}")
    
    api_config <- purrr::map(c(1:3),
                              .f = \(a){
                                glue::glue("{names(x$gbif)[a]}: {x$gbif[[a]]}")
                              }) |>
      unlist()
    names(api_config) <- rep("*", 3)
    cli::cli_bullets(api_config)
  }
}