#' Build a Darwin Core Archive from a Biodiversity Data Package
#' 
#' `write_dwca()` and `read_dwca()` are antinyms. `write_dwca()` and `build()` 
#' are synonyms, with the former mirroring `readr` syntax and the latter 
#' `devtools` syntax (see also `check.dwca()`).
#' @param pkg A directory containing a Biodiversity Data Package. Defaults to
#' current working directory.
#' @param path (Optional) Path to the resulting zip file. Defaults to `NULL`, 
#' indicating that the file should be saved to the current parent directory.
#' @return Called exclusively for the side-effect of building a 'Darwin Core 
#' Archive' (i.e. a zip file); doesn't return anything to the workspace.
#' @importFrom glue glue
#' @importFrom pkgload pkg_name
#' @importFrom readr write_csv
#' @importFrom xml2 write_xml
#' @importFrom zip zip
#' @export
build_dwca <- function(pkg = ".",
                       path = NULL) {
  # setup
  check_bd_package_contents()
  temp_loc <- tempdir() # create file structure in temporary directory
  package_name <- pkg_name()
  working_file <- dir.create(glue("{temp_loc}/{package_name}"))
  # work from within the specified package
  local_project(pkg) # check only within this package
  
  # read information in `data` & export to csv
  data_files <- list.files("data", pattern = ".rda$")
  file_prefix <- sub(".rda$", "", data_files)
  lapply(file_prefix, function(x){
    df <- readRDS(glue("{x}.rda"))
    write_csv(df, file = glue("{working_file}/{x}.csv"))
  }) |>
    invisible()
  
  # read in metadata & and export to eml.xml
  read_md("./vignettes/metadata.Rmd") |>
    write_xml(file = glue("{working_file}/eml.xml"))
  
  # read in or create schema and export to metadata.xml
  if(!file.exists("./vignettes/schema.Rmd")){
    use_bd_schema()
  }
  read_md("./vignettes/schema.Rmd") |>
    write_xml(file = glue("{working_file}/metadata.xml"))
  
  # save as zip file in requested location
  if(is.null(path)){
    path <- getwd() |>
      sub("/[[:alpha:]]+$", "", x = _)    
  }
  target_file <- glue("{path}/{package_name}.zip")
  zip::zip(zipfile = target_file, 
           files = list.files(working_file),
           mode = "cherry-pick")
  unlink(working_file)
}