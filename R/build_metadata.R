#' Create a metadata statement for a Darwin Core Archive
#' 
#' A metadata statement lists the owner of the dataset, how it was collected,
#' and how it may used (i.e. its' licence). This function simply converts
#' metadata stored in a markdown file to xml, and stores it in the folder 
#' specified using the `directory` argument.
#' 
#' This function is a fairly shallow wrapper on top of functionality build
#' in the `elm` package, particularly `read_elm()` and `write_elm()`. You can 
#' use that package to gain greater control, or to debug problems, should you 
#' wish.
#' @param file Path to a metadata statement stored in markdown format (.md).
#' @param directory A folder to place the resulting file. Defaults to `data`.
#' @returns Does not return an object to the workspace; called for the side
#' effect of building a file named `meta.xml` in the `data` directory.
#' @importFrom elm add_elm_header
#' @importFrom elm read_elm
#' @importFrom elm write_elm
#' @export
build_metadata <- function(x = "data", 
                           file = "./data/eml.xml"){
  if(!file.exists(x)){
    abort("`x` doesn't exist in specified location.")
  }
  # import file, ensure EML metadata is added, convert to XML
  read_elm(x) |>
    add_elm_header() |>
    write_elm(file = file)
}