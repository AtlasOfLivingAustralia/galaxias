#' Build a Darwin Core Archive from a `dwca` object
#' @param .dwca A `dwca` object
#' @param path Name of the zip file
#' @return No object is returned; this function is called for the side-effect
#' of building a 'Darwin Core Archive' (i.e. a zip file)
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom rlang inform
#' @importFrom xml2 write_xml
#' @importFrom zip zip
#' @export
build_dwca <- function(.dwca,
                       file = "dwca.zip") {
  # convert `metadata` slot to be named `eml`
  nmz <- names(.dwca)
  if(any(nmz == "metadata")){
    nmz[nmz == "metadata"] <- "eml"
  }
  names(.dwca) <- nmz
  
  # add `schema`
  .dwca$meta <- build_schema(.dwca)
  
  # create a temporary directory to store objects
  temp_dir <- tempdir()
  temp_loc <- Sys.time() |> 
    as.character() |>
    gsub("\\s", "-", x = _)
  store_dir <- glue("{temp_dir}/galaxias-{temp_loc}")
  dir.create(store_dir)
  
  # loop across objects, saving the correct type to the correct name
  object_names <- names(.dwca)
  for(i in seq_along(object_names)){
    obj <- .dwca[[i]]
    file_name <- object_names[i]
    if(inherits(obj, "data.frame")){
      write_csv(obj, file = glue("{store_dir}/{file_name}.csv"))
    }else{
      write_xml(obj, file = glue("{store_dir}/{file_name}.xml"))
    }
  }
  all_files <- list.files(store_dir)
  
  # build zip file
  inform(glue("Building {file}"))
  zip::zip(zipfile = file, 
           files = glue("{store_dir}/{all_files}"),
           mode = "cherry-pick")
  inform("Cleaning temporary directory")
  unlink(store_dir)
}
