#' Validate a Darwin Core Archive via API
#' 
#' System not ready
#' @noRd
#' @keywords Internal
publish_dwca <- function(pkg = ".", 
                         file = NULL){
  galaxias_API(pkg = pkg, 
               file = file, 
               api = "publish")
}