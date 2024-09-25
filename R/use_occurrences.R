#' Add occurrence-specific information to a `tibble`
#' 
#' @description
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
    .df,
    occurrenceID = NULL,
    basisOfRecord = NULL,
    occurrenceStatus = NULL,
    # recordNumber = NULL, # keep?
    .keep = "unused",
    .messages = TRUE
){
  if(missing(.df)){
    abort(".df is missing, with no default.")
  }
  
  fn_args <- ls()
  
  # capture arguments as a list of quosures
  # NOTE: enquos() must be listed alphabetically
  fn_quos <- enquos(basisOfRecord, occurrenceID, occurrenceStatus)
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
  
  # if used in occurrenceID, run `use_id_random()`
  mc <- match.call(expand.dots = FALSE)
  
  if(!is.null(mc$occurrenceID)) {
    if(mc$occurrenceID == "use_id_random()") {

      check_uuid_exists(.df)

      result <- .df |>
        mutate(
          occurrenceID = use_id_random()
        )
    }
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
  
  if(isTRUE(.messages)) {
    if(length(matched_cols > 0)) {
      col_progress_bar(cols = matched_cols)
    }
  }
  
  # run column checks
  check_basisOfRecord(result, level = "abort")
  check_occurrenceID(result, level = "abort")
  check_occurrenceStatus(result, level = "abort")
  
  return(result)
}

#' Check whether a UUID column is already present in a dataset
#' @rdname check_dwc
#' @param df Data frame or tibble passed by user
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @importFrom uuid as.UUID
#' @importFrom purrr map
#' @keywords Internal
#' @noRd
check_uuid_exists <- function(df, 
                              # level = c("inform", "warn", "abort"),
                              call = caller_env()
){
  # get first sample of df values, test whether any are UUIDs
  df_test_uuid <- purrr::map(head(df, 10L), uuid::as.UUID) |> bind_rows()
  
  
  if(any(!is.na(df_test_uuid))) {
    
    uuid_cols <- df_test_uuid[sapply(df_test_uuid, function(x) any(!is.na(x)))] |>
      names()
    
    bullets <- c(
      "Column {.field {uuid_cols}} contains UUID values.",
      i = "Existing unique ID values should be used if they have already been generated.",
      i = "Use `use_occurrences(occurrenceID = {.field {uuid_cols}})` instead."
    ) |>
      cli::cli_bullets() |>
      cli::cli_fmt()
    
    cli::cli_abort(bullets, call = call)
  }
}

#' Create a random identifier column
#' 
#' @description
#' Uses `uuid::UUIDgenerate()` to create a random UUID code without the possible 
#' shortfalls of being influenced by R's internal random number generators 
#' (i.e., set.seed). 
#' 
#' @param x A vector
#' @importFrom uuid UUIDgenerate
#' @importFrom dplyr n
#' @export
use_id_random <- function(x) {
  if(missing(x)) {
    uuid::UUIDgenerate(use.time = TRUE, dplyr::n())
  } else {
    cli_abort("{.code use_id_random()} must be used in `use_occurrences()`.")
    # vctrs::vec_rank(x, ties = "sequential", incomplete = "na")
  }
}

#' Create a composite identifier from two or more columns, separated by a colon.
#' FIXME: This function doesn't not work at the moment.
#' TODO: Question:: Would a composite identifier column be called occurrenceID?
#' Should be globally unique - it may be necessary to still add a random number?
#' Depends on what columns are used to build it.
#' @param cols character vector of columns to use
#' @keywords Internal
#' @noRd
use_id_composite <- function(data,
                             cols = NULL) {
  # TODO: check if the columns specified are contained / found in the data
  # if not, warning and check for spelling etc.
  # This method assumes string values - unchecked with other column types
  concatenated_values <- apply(data[cols], 1, function(row) {
    gsub(" ", "", tolower(paste(row, collapse = ":")))
  })
  # TODO: For now just adding a sequential numeric value
  data$occurrenceID <- paste0(concatenated_values, ":", 1:nrow(data))
  return(invisible(data))
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
      check_contains_values(values = valid_basisOfRecord(), 
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

#' @rdname check_dwc
#' @order 3
#' @importFrom dplyr select
#' @export
check_occurrenceID <- function(.df, 
                               level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "occurrenceID")){
    .df |>
      select("occurrenceID") |>
      check_unique(level = level)
  }
}

#' Check occurrenceStatus field is valid
#' @rdname check_dwc
#' @param level what action should the function take for non-conformance? 
#' Defaults to `"inform"`.
#' @order 4
#' @export
check_occurrenceStatus <- function(.df, 
                                level = c("inform", "warn", "abort")
){
  level <- match.arg(level)
  if(any(colnames(.df) == "occurrenceStatus")){
    .df |>
      select("occurrenceStatus") |>
      check_is_string(level = level) |>
      check_contains_values(values = c("present", "absent"), 
                            level = level)
  }
  
  .df
}