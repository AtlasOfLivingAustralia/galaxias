#' Add occurrence-specific information to a `tibble`
#' 
#' Format fields uniquely identify each occurrence record and specify the type 
#' of record. `occurrenceID` and `basisOfRecord` are necessary fields of 
#' information for occurrence records, and should be appended to a data set
#' to conform to Darwin Core Standard prior to submission.
#' 
#' In practice this is no different from using `mutate()`, but gives some 
#' informative errors, and serves as a useful lookup for fields in 
#' the Darwin Core Standard.
#' @param df a `data.frame` or `tibble` that the column should be appended to.
#' @param occurrenceID A character string. Every occurrence should have an 
#' `occurrenceID` entry. Ideally IDs should be persistent to avoid being lost 
#' in future updates. They should also be unique, both within the dataset, and 
#' (ideally) across all other datasets.
#' @param basisOfRecord Record type. Only accepts `camelCase`, for 
#' consistency with field names. 
#' Accepted `basisOfRecord` values are one of:
#' * `"humanObservation"`, `"machineObservation"`, `"livingSpecimen"`, 
#' `"preservedSpecimen"`, `"fossilSpecimen"`, `"materialCitation"`
#' @param .keep Control which columns from .data are retained in the output. 
#' Note that unlike `dplyr::mutate`, which defaults to `"all"` this defaults to 
#' `"unused"`, which only keeps Darwin Core fields and not those fields used to 
#' generate them.
#' @returns A tibble with the requested fields added.
#' @details
#' Examples of `occurrenceID` values:
#' * `000866d2-c177-4648-a200-ead4007051b9`
#' * `http://arctos.database.museum/guid/MSB:Mamm:233627`
#' 
#' Accepted `basisOfRecord` values are one of:
#' * `"humanObservation"`, `"machineObservation"`, `"livingSpecimen"`, 
#' `"preservedSpecimen"`, `"fossilSpecimen"`, `"materialCitation"`
#' 
#' 
#' @importFrom dplyr mutate
#' @importFrom rlang abort
#' @export
use_occurrences <- function(
    df,
    occurrenceID = NULL,
    basisOfRecord = NULL,
    # recordNumber = NULL, # keep?
    .keep = "unused"
){
  if(missing(df)){
    abort("df is missing, with no default.")
  }
  
  mc <- match.call(expand.dots = FALSE)
  
  if(mc$occurrenceID == "use_id_random()") {
    
    check_uuid_exists(df)
    
    result <- df |>
      mutate(
        occurrenceID = use_id_random()
      )
  } 
  
  result <- df |>
    mutate(occurrenceID = {{occurrenceID}},
           basisOfRecord = {{basisOfRecord}},
           # recordNumber = {{recordNumber}},
           .keep = .keep)
  
  check_basisOfRecord(result, level = "abort")
  
  result
}

#' Check whether a UUID column is already present in a dataset
#' @rdname check_dwc
#' @param df Data frame or tibble passed by user
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @keywords Internal
#' @noRd
check_uuid_exists <- function(df, 
                              # level = c("inform", "warn", "abort"),
                              call = caller_env()
){
  # get first sample of df values, test whether any are UUIDs
  df_test_uuid <- sapply(head(df, 10L), uuid::as.UUID) |> as_tibble()
  
  
  if(any(!is.na(df_test_uuid))) {
    
    uuid_cols <- df_test_uuid[sapply(df_test_uuid, function(x) any(!is.na(x)))] |>
      names()
    
    bullets <- c(
      "Column {.field {uuid_cols}} contains UUID values.",
      i = "Use `use_occurrences(occurrenceID = {.field {uuid_cols}})` instead."
    ) |>
      cli::cli_bullets() |>
      cli::cli_fmt()
    
    cli::cli_abort(bullets, call = call)
  }
}

#' Check basisOfRecord field is valid
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 4
#' @export
check_basisOfRecord <- function(.df, 
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "basisOfRecord")){
    .df |>
      select("basisOfRecord") |>
      check_is_string(level = level) |>
      check_contains_values(y = valid_basisOfRecord(), 
                            level = level)
  }
  .df
}

#' Accepted values for `basisOfRecord`
#' @noRd
#' @keywords Internal
valid_basisOfRecord <- function(){
  c("humanObservation", 
    "machineObservation",
    "livingSpecimen",
    "preservedSpecimen",
    "fossilSpecimen",
    "materialCitation")
}