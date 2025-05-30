#' @param \dots Additional arguments, currently ignored.
#' @rdname check_archive
#' @order 4
#' @export
print.gbif_validator <- function(x, 
                                 ...){
  cli::cli_h3("Object of class {.cls gbif_validator}")
  print_archive_status(x)
  cli::cli_bullets(c("i" = "Use `view_report(x)` for more information"))
}