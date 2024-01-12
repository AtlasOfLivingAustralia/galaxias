#' Add information to a `dwca` object
#' 
#' These are all pipable functions for adding relevant information to a `dwca`
#' object. The underlying workhorse function is `add_data()`, but it is 
#' recommended to use the more specific functions `add_occurrences()`,
#' `add_events()` and `add_media()`. `add_metadata()` is also mandatory and 
#' requires either an `xml` object/file or a markdown file (`rmd` or `md` 
#' preferred). `add_schema()` is called internally and there is no
#' requirement for the user to call it, but it is shipped here as it may be 
#' useful for debugging purposes.
#' @param .dwca An object of class `dwca`, constructed with `dwca()`.
#' @param df Object of class `tibble` or `data.frame`.
#' @returns An object of class `dwca`, updated with the provided information.
#' @name add_
#' @order 1
#' @importFrom rlang abort
add_occurrences <- function(.dwca,
                            df = NULL
                            ){
  check_tibble(df)
  add_data(.dwca, df = df, slot = "occurrences")
}

#' @rdname add_
#' @order 2
#' @export
add_events <- function(.dwca,
                       df = NULL
){
  check_tibble(df)
  add_data(.dwca, df = df, slot = "events")
} 

#' @rdname add_
#' @order 3
#' @export
add_media <- function(.dwca,
                       df = NULL
){
  check_tibble(df)
  add_data(.dwca, df = df, slot = "media")
} 

#' @name add_
#' @param xml Object of class `xml_document`, such as imported from 
#' `.xml` with `xml2::read_xml()` or from markdown (`.rmd`) using 
#' `galaxias::read_md()`.
#' @order 4
#' @importFrom xml2 read_xml
#' @export
add_metadata <- function(.dwca,
                         xml = NULL){
  check_xml(xml)
  update_dwca(.dwca, list(metadata = xml))
}

#' @rdname add_
#' @param slot Name of the slot where this data should go.
#' @order 5
#' @export
add_data <- function(.dwca, 
                     df = NULL,
                     slot = NULL){
  if(!is.null(df)){
    obj <-  df
  }else{
    obj <- NULL
  }
  obj_list <- list(obj)
  names(obj_list) <- slot
  # add to object
  update_dwca(.dwca, obj_list)
}

#' @rdname add_
#' @order 6
#' @export
add_schema <- function(.dwca){
  obj <- list(schema = build_schema(.dwca))
  update_dwca(.dwca, obj)
}

#' internal function to sequentially add data to `dwca` object
#' @param x named list to be appended to the object
#' @noRd
#' @keywords Internal
update_dwca <- function(.dwca, x){
  result <- append(.dwca, x)
  class(result) <- "dwca"
  result
}

#' Internal function to check a tibble is present, and that it is, in fact, a tibble
#' @noRd
#' @keywords Internal
check_tibble <- function(df){
  if(is.null(df)){
    abort("`df` is missing, with no default")
  }
  if(!inherits(df, "data.frame")){
    abort("`df` must be a `data.frame` or `tibble`")
  }
}

#' Internal function to check a tibble is present, and that it is, in fact, a tibble
#' @noRd
#' @keywords Internal
check_xml <- function(xml){
  if(is.null(xml)){
    abort("`xml` is missing, with no default")
  }
  if(!inherits(df, "xml_document")){
    abort("`df` must be an `xml_document`")
  }
}