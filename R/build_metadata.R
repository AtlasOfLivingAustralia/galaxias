#' Create a metadata statement for a Darwin Core Archive
#' 
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it may used (i.e. its' licence). This function simply converts
#' metadata stored in a markdown file to xml, and stores it in the `data` 
#' folder.
#' @param file path to a metadata statement stored in markdown format (.md).
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the `data` directory.
#' @importFrom elm add_eml_row
#' @importFrom elm read_md_chr
#' @importFrom elm as_md_xml
#' @importFrom elm write_md_xml
#' @export
build_metadata <- function(file) {
  
  # check file is present
  if(missing(file)){
    abort("`file` is missing, with no default.")
  }
  if(!file.exists(file)){
    abort("`file` doesn't exist in specified location.")
  }
  
  # import file, ensure EML metadata is added, convert to XML
  result <- read_md_chr(file) |>
    add_eml_row() |>
    as_md_xml()
  
  # check if `data` folder is present
  if(!file.exists("data")){
    use_directory("data")
  }
  
  # save XML to `data/eml.xml`
  write_md_xml(result, 
               file = glue("data/eml.xml"))
}