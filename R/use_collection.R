#' Add museum- or collection-specific information to a `tibble`
#' 
#' Format fields that specify the collection or catelog number of a 
#' specimen or occurrence record.
#' 
#' In practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for fields in 
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param datasetID An identifier for the set of data. May be a global unique 
#' identifier or an identifier specific to a collection or institution.
#' @param datasetName The name identifying the data set from which the record 
#' was derived.
#' @param catalogNumber A unique identifier for the record within the data set 
#' or collection.
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`; i.e. only keeps Darwin Core fields, and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `datasetID` values:
#' * `b15d4952-7d20-46f1-8a3e-556a512b04c5`
#' 
#' Examples of `datasetName` values:
#' * `Grinnell Resurvey Mammals`
#' * `Lacey Ctenomys Recaptures`
#' 
#' Examples of `catalogNumber` values:
#' * `145732`
#' * `145732a`
#' * `2008.1334`
#' * `R-4313`
#' 
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_collection <- function(
    df,
    datasetID = NULL,
    datasetName = NULL,
    catalogNumber = NULL,
    .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default")
  }
  result <- df |>
    mutate(datasetID = {{datasetID}},
           datasetName = {{datasetName}},
           catalogNumber = {{catalogNumber}},
           .keep = .keep)
  
  result
}