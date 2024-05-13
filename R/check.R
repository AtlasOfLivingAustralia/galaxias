#' Check package directories are correctly specified
#' 
#' Called by `build_dwca()`
#' @importFrom devtools is.package
#' @importFrom rlang abort
#' @importFrom usethis local_project
#' @noRd
#' @keywords Internal
check_bd_package_contents <- function(pkg){
  
  # check that object is a package
  if(!is.package(pkg)){
    bullets <- c("Specified directory is not a package",
                 i = "use `create_bd_package()` to set up a package")
    abort(bullets)
  }
  
  local_project(pkg) # check only within this package
  
  # check whether data is present
  if(!file.exists("data")){
    bullets <- c("`data` directory is required, but missing",
                 i = "use `usethis::use_data()` to add data to your package")
    abort()
  }
  
  # run checks to determine whether usable data is present
  if(!file.exists("./data/occurrences.rda")){
    bullets <- c("`occurrences.rda` is required, but missing from `data`",
                 i = "use `add_bd_data_raw()` for examples of how to add raw data to your package",
                 i =  "use `usethis::use_data()` to add data to your package")
    abort(bullets)
  }    
  
  # run checks to determine whether metadata is present
  if(!file.exists("./vignettes/metadata.Rmd")){
    bullets <- c("`metadata.Rmd` is required, but missing from `vignettes`",
                 i = "use `use_bd_metadata()` to create a boilerplate metadata statement")
    abort(bullets)
  }
  
  # run checks to determine whether metadata is present
  if(!file.exists("./vignettes/schema.Rmd")){
    bullets <- c("`metadata.Rmd` is required, but missing from `vignettes`",
                 i = "use `use_bd_metadata()` to create a boilerplate metadata statement")
    abort(bullets)
  }
}


#' check a vector consists only of values in a second vector
#' Intended to be called by other `check_` functions
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_contains <- function(x, y, level){
  x_lookup <- x %in% y
  if(any(!x_lookup)){
    unexpected_values <- x[!x_lookup]
    unexpected_string <- glue_collapse(glue("`{unexpected_values}`"),
                                       sep = ", ",
                                       last = " & ")     
    accepted_string <- glue_collapse(glue("`{y}`"),
                                     sep = ", ",
                                     last = " or ")
    bullets <- c(glue("Unexpected value(s) provided: {unexpected_string}"),
                 i = glue("Accepted values are {accepted_string}"))
    do.call(level, list(message = bullets))
  }
}

#' check a vector is a string
#' Intended to be called by other `check_` functions
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_is_string <- function(x){
  if(!inherits(x, "character")){
    abort("Supplied value is not a string")
  }
}