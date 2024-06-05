#' Add `data` folder to a biodiversity data project
#' 
#' This function places specified objects in the `data` folder as `.csv` files.
#' Note that this is very different from `usethis::use_data()` which uses `.rda`
#' format (and then only when `internal = FALSE`). This is provided for 
#' consistency with `usethis`, but a more flexible approach is simply to use
#' `readr::write_csv()`.
#' @param ... Unquoted names of an object to save.
#' @param overwrite (logical) Should existing objects be overwritten? Defaults
#' to FALSE.
#' @importFrom glue glue
#' @importFrom rlang as_label
#' @importFrom rlang enquos
#' @export
use_bd_data <- function(..., overwrite = FALSE){
  use_directory("data")
  dots <- enquos(...)[[1]]
  path <- glue("data/{as_label(dots)}.csv")
  if(file.exists(path)){
    if(overwrite){
      inform(glue("overwriting existing file: {path}"))
      write_csv(x = eval_tidy(dots), file = path)
    }else{
      bullets <- c(glue("file already exists at {path}"),
                   i = "to replace, set `overwrite = TRUE`")
      inform(bullets)
    }
  }else{
    inform(glue("saving to {path}"))
    write_csv(x = eval_tidy(dots), file = path)  
  }
}

#' Add `data-raw` folder to a biodiversity data project
#' 
#' Add a script to `data-raw` with example code of how to rename/select/relocate 
#' fields.
#' @importFrom usethis use_directory 
#' @importFrom usethis use_template
#' @export
use_bd_data_raw <- function(){
  use_directory("data-raw")
  use_template(template = "data_manipulation_script.R",
               save_as = "data-raw/data_manipulation_script.R",
               package = "galaxias")
}

#' Add `DESCRIPTION` to a biodiversity data project
#' 
#' In a biodiversity data project, it is possible to use a DESCRIPTION file to 
#' add authorship and licencing information. This can be useful, for example,
#' if you require the data and repository to have different licences.
#' @importFrom usethis use_description
#' @export
use_bd_description <- function(){
  use_description(fields = list(
    "Title" = "Title of this Data Package (One Line, Title Case)",
    "Description" = "Description of this data package (one paragraph).",
    "Licence" = "`use_ccby_licence()` (recommended), `use_cc0_licence()` or friends to pick a licence appropriate for a data package"))
}

#' Add a metadata statement to a biodiversity data project
#' 
#' Builds a file called `metadata.md`, for storing information on your dataset.
#' This provides a convenient document structure to describe what your data is, 
#' who collected it, and what licence it is released under.
#' @importFrom usethis use_directory
#' @export
use_bd_metadata <- function(){
  use_template(template = "pkg-metadata",
               save_as = "metadata.md",
               package = "galaxias")
}

#' Add `README` to a biodiversity data project
#' 
#' This function adds `galaxias`-specific `README` instead of the `usethis` 
#' default.
#' @name use_bd_readme
#' @importFrom usethis use_template
#' @export
use_bd_readme_md <- function(){
  use_template(template = "project-README",
               save_as = "README.md",
               package = "galaxias")
}

#' @rdname use_bd_readme
#' @importFrom usethis use_template
#' @export
use_bd_readme_rmd <- function(){
  use_template(template = "pkg-README",
               save_as = "README.Rmd",
               data = list("Project" = pkg_name()),
               package = "galaxias")
}