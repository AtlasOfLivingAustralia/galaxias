#' Internal function to report at the requested severity level
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom rlang abort 
#' @noRd
#' @keywords Internal
switch_check <- function(level = "inform", 
                         bullets = ""){
  switch(level, 
         "inform" = inform(bullets),
         "warn" = warn(bullets),
         "abort" = abort(bullets))
  invisible(FALSE)
}

#' check a vector consists only of values in a second vector
#' @param x vector of values
#' @param y vector against which x should be compared
#' @noRd
#' @keywords Internal
check_contains <- function(x, 
                           y, 
                           level = "inform"){
  x_lookup <- x %in% y
  if(any(!x_lookup)){
    unexpected_values <- x[!x_lookup]
    unexpected_string <- glue_collapse(glue("`{unexpected_values}`"),
                                       sep = ", ",
                                       last = " & ")     
    accepted_string <- glue_collapse(glue("`{y}`"),
                                     sep = ", ",
                                     last = " or ")
    bullets <- c(glue("Unexpected value(s) provided: {unexpected_string}"),
                 i = glue("Accepted values are {accepted_string}"))
    switch_check(level, bullets)
  }else{
    invisible(TRUE)
  }
}

#' check a vector is numeric
#' @noRd
#' @keywords Internal
check_is_numeric <- function(x, 
                             level = "inform"){
  if(!inherits(x, c("numeric", "integer"))){
    bullets <- c(i = "Supplied value is not numeric")
    switch_check(level, bullets)
  }else{
    invisible(TRUE)
  }
}

#' check a vector is a string
#' @noRd
#' @keywords Internal
check_is_string <- function(x, 
                            level = "inform"){
  if(!inherits(x, "character")){
    bullets <- c(i = "Supplied value is not a string")
    switch_check(level, bullets)
  }else{
    invisible(TRUE)
  }
}

#' check a vector has one row per value
#' @noRd
#' @keywords Internal
check_unique <- function(x,
                         level = "inform"){
  unique_check <- length(unique(x)) == length(x)
  if(!unique_check){
    bullets <- c(i = "Supplied field does not contain a unique value in each cell")
    switch_check(level, bullets)
  }else{
    invisible(TRUE)
  }
}

#' check a vector is within a specified range
#' @noRd
#' @keywords Internal
check_within_range <- function(x, 
                               level = "inform", 
                               lower, 
                               upper){
  if(!all(x >= lower & x <= upper)){
    bullets <- c(i = "Supplied value is not within requested range")
    switch_check(level, bullets)
  }else{
    invisible(TRUE)
  }
}