#' Internal function to report at the requested severity level
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom rlang abort
#' @importFrom rlang caller_env
#' @noRd
#' @keywords Internal
switch_check <- function(level = "inform", 
                         bullets = "", 
                         call = caller_env()){
  switch(level, 
         "inform" = inform(bullets, call = call),
         "warn" = warn(bullets, call = call),
         "abort" = abort(bullets, call = call))
}

#' Internal function used to catch errors in low-level `check_` functions
#' As these are only ever called internally, this is basically a debugging assistant.
#' @importFrom rlang abort
#' @noRd
#' @keywords internal
check_data_frame <- function(.df,
                             call = caller_env()
){
  if(!inherits(.df, "data.frame")){
    abort("objects supplied to `check_` functions must be a `tibble` or `data.frame`.",
          call = call)
  }
  if(ncol(.df) > 1){
    abort("`data.frame`s supplied to `check_` functions should only have one column.",
          call = call)
  }
  .df
}

#' check a vector consists only of values in a second vector
#' @param x vector of values
#' @param y vector against which x should be compared
#' @importFrom dplyr pull
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
check_contains <- function(.df, 
                           y, 
                           level = "inform",
                           call = caller_env()
                           ){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> 
    pull(field_name) |>
    unique() |>
    sort()
  x_lookup <- x %in% y
  if(any(!x_lookup)){
    unexpected_values <- x[!x_lookup]
    unexpected_string <- glue_collapse(glue("{unexpected_values}"),
                                       sep = ", ",
                                       last = " & ")     
    accepted_string <- glue_collapse(glue("{y}"),
                                     sep = ", ",
                                     last = " or ")
    bullets <- c(glue("`{field_name}` contains unexpected value(s): {unexpected_string}"),
                 i = glue("Accepted values are {accepted_string}"))
    switch_check(level, 
                 bullets,
                 call = call)
  }
  .df
}

#' check a vector is numeric
#' @noRd
#' @keywords Internal
check_is_numeric <- function(.df, 
                             level = "inform",
                             call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  if(!inherits(x, c("numeric", "integer"))){
    bullets <- c(i = glue("`{field_name}` is not numeric"))
    switch_check(level, 
                 bullets,
                 call = call)
  }
  .df
}

#' check a vector is a string
#' @noRd
#' @keywords Internal
check_is_string <- function(.df,
                            level = "inform",
                            call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  if(!inherits(x, "character")){
    bullets <- c(i = glue("`{field_name}` is not a string"))
    switch_check(level, 
                 bullets,
                 call = call)
  }
  .df
}

#' check a vector has one row per value
#' @noRd
#' @keywords Internal
check_unique <- function(.df,
                         level = "inform",
                         call = caller_env()
){
  check_data_frame(.df)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  unique_check <- length(unique(x)) == length(x)
  if(!unique_check){
    bullets <- c(i = glue("`{field_name}` does not contain a unique value in each cell"))
    switch_check(level, 
                 bullets,
                 call = call)
  }
  .df
}

#' check a vector is within a specified range
#' @noRd
#' @keywords Internal
check_within_range <- function(.df, 
                               level = "inform", 
                               lower,
                               upper,
                               call = caller_env()
){
  .df |> 
    check_data_frame() |>
    check_is_numeric(level = level)
  field_name <- colnames(.df)[[1]]
  x <- .df |> pull(field_name)
  range_check <- (x >= lower & x <= upper)
  if(!all(range_check)){
    bullets <- c(i = glue("`{field_name}` contains values ouside of {lower} <= x <= {upper}"))
    switch_check(level,
                 bullets,
                 call = call)
  }
  .df
}