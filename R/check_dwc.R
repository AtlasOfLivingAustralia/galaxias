#' Check for Darwin Core field conformance
#' 
#' Function to check whether a data.frame or tibble conforms to DwC standards
#' NOTE: Option to use `bdc` for checks, and/or `pointblank` for running them
#' @param df A tibble against which checks should be run
#' @importFrom rlang inform
#' @importFrom cli cli_bullets
#' @param df a tibble containing data
#' @order 1
#' @export
check_dwc <- function(.df){
  # dwc_terms
  fields <- colnames(.df)
  available_checks <- c("occurrenceID",
                        "basisOfRecord",
                        "decimalLatitude",
                        "decimalLongitude")
  checkable_fields <- fields[fields %in% available_checks]
  check_functions <- c("check_fields",
                       glue("check_{checkable_fields}"))
  cli_bullets(c(">" = "Checking DwC fields."))
  # run each function on df
  lapply(check_functions, 
         function(x){do.call(x, args = list(.df = .df))}) |>
    invisible()
}

#' Check for non DwC fields
#' NOTE: should probably swap to check_contains() for this.
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom rlang warn
#' @rdname check_dwc
#' @order 2
#' @export
check_fields <- function(.df, 
                         level = c("inform", "warn", "abort")){
  level <- match.arg(level)
  result <- tibble(dwc_terms = colnames(.df)) |>
    check_contains_terms(y = dwc_terms,
                         level = level)
  .df
}

#' Check package directories are correctly specified
#' 
#' Called by `build_dwca()`
#' @importFrom rlang abort
#' @importFrom usethis local_project
#' @noRd
#' @keywords Internal
check_bd_package_contents <- function(pkg){
  
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
