#' Add taxonomic information to a `tibble`
#' 
#' Format fields that contain taxonomic name information from kingdom to 
#' species, as well as the common/vernacular name, to a `tibble`.
#' 
#' In 
#' practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for taxonomic names in 
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param kingdom The kingdom name of identified taxon.
#' @param phylum The phylum name of identified taxon.
#' @param class The class name of identified taxon.
#' @param order The order name of identified taxon.
#' @param family The family name of identified taxon.
#' @param genus The genus name of the identified taxon.
#' @param species The species name of the identified taxon.
#' @param specificEpithet The name of the first species or species epithet of 
#' the `scientificName`. 
#' [See documentation](https://dwc.tdwg.org/list/#dwc_specificEpithet) 
#' @param vernacularName The common or vernacular name of the identified taxon.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `specificEphithet`:
#' * If `scientificName` is `Abies concolor`, the `specificEpithet` is `concolor`.
#' * If `scientificName` is `Semisulcospira gottschei`, the `specificEpithet` is `gottschei`.
#' 
#' @seealso [use_scientific_name()] for adding `scientificName` and authorship information.
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_taxonomy <- function(
    .df,
    kingdom = NULL,
    phylum = NULL,
    class = NULL,
    order = NULL,
    family = NULL,
    genus = NULL,
    species = NULL,
    specificEpithet = NULL,
    vernacularName = NULL,
    .keep = "unused"
){
  if(missing(.df)){
    abort("df is missing, with no default")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(class, family, genus, kingdom, order, phylum, species, specificEpithet, vernacularName)
  names(fn_quos) <- fn_args
  
  # find arguments that are NULL but exist already in `df`
  # then remove their names before `mutate()`
  # otherwise, these DwC columns are deleted by `mutate(.keep = "unused")` 
  fn_quo_is_null <- fn_quos |> 
    purrr::map(\(user_arg)
               rlang::quo_is_null(user_arg)) |> 
    unlist()
  
  null_col_exists_in_df <- fn_quo_is_null & (names(fn_quos) %in% colnames(.df))
  
  if(any(null_col_exists_in_df)){
    fn_quos <- fn_quos |> 
      purrr::keep(!names(fn_quos) %in% names(which(null_col_exists_in_df)))
  }
  
  # Update df
  result <- .df |> 
    mutate(!!!fn_quos, 
           .keep = .keep)
  
  check_missing_all_args(fn_call = match.call(), 
                         fn_args = fn_args, 
                         user_cols = colnames(result))
  
  # inform user which columns will be checked
  matched_cols <- names(result)[names(result) %in% fn_args]
  col_progress_bar(cols = matched_cols)
  
  # run column checks
  # Q: Should taxonomic names be validated in galaxias?
  #    Would a separate taxonomic checking package be worthwhile?
  check_kingdom(result, level = "abort")
  check_phylum(result, level = "abort")
  check_class(result, level = "abort")
  check_order(result, level = "abort")
  check_family(result, level = "abort")
  check_genus(result, level = "abort")
  check_species(result, level = "abort")
  check_specificEpithet(result, level = "abort")
  check_vernacularName(result, level = "abort")
  
  return(result)
}

#' Check kingdom field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_kingdom <- function(.df, 
                          level = c("inform", "warn", "abort")
                          ){
  level <- match.arg(level)
  if(any(colnames(.df) == "kingdom")){
    .df |>
      select("kingdom") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check phylum field is valid
#'
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_phylum <- function(.df, 
                         level = c("inform", "warn", "abort")
                         ){
  level <- match.arg(level)
  if(any(colnames(.df) == "phylum")){
    .df |>
      select("phylum") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check class field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_class <- function(.df, 
                        level = c("inform", "warn", "abort")
                        ){
  level <- match.arg(level)
  if(any(colnames(.df) == "class")){
    .df |>
      select("class") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check order field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_order <- function(.df, 
                        level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "order")){
    .df |>
      select("order") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check family field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_family <- function(.df, 
                         level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "family")){
    .df |>
      select("family") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check genus field is valid
#'
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_genus <- function(.df, 
                        level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "genus")){
    .df |>
      select("genus") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check species field is valid
#'
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_species <- function(.df, 
                          level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "species")){
    .df |>
      select("species") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check specificEpithet field is valid
#' 
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_specificEpithet <- function(.df, 
                                  level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "specificEpithet")){
    .df |>
      select("specificEpithet") |>
      check_is_string(level = level) |>
      check_word_number(max_n_word = 1,
                        level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string

#' Check vernacularName field is valid
#'
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 7
#' @export
check_vernacularName <- function(.df, 
                                 level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "vernacularName")){
    .df |>
      select("vernacularName") |>
      check_is_string(level = level)
  }
  .df
}
# TODO: Currently only checks whether input is a string
