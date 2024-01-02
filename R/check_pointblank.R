#' pointblank-based checking of a `dwca` object
#' 
#' @export
check <- function(.dwca){
  x <- .dwca$data
  x |>
    create_agent() |>
    col_exists(columns = vars("decimalLatitude", "something"),
               label = "Check variables exist") |>
    check_decimalLongitude() |>
    interrogate()
  
}

#' check for decimalLongitude
#' @noRd
#' @keywords Internal
check_decimalLongitude <- function(object){
  object |>
    specially(fn = function(a){
      if(any(colnames(a) == "decimalLongitude")){
        test_col_vals_between(object = a,
                              columns = "decimalLongitude",
                              left = -180,
                              right = 180)
      }else{
        FALSE
      }
    },
    label = "Check decimalLongitude",
    brief = "If decimalLongitude exists, test values are valid") 
}

# ## test this:
# .dwca <- list(
#    data = tibble(decimalLatitude = 10)) 
# .dwca |>
#   galaxias::check()