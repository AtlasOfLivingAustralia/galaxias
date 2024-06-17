#' @noRd
#' @keywords Internal
parse_xml_to_md <- function(x){
  x |>
    parse_xml_to_list() |>
    parse_list_to_tibble() |>
    parse_tibble_to_md()
}

#' @importFrom xml2 as_list
#' @noRd
#' @keywords Internal
parse_xml_to_tibble <- function(x){
  x |>
    parse_xml_to_list() |>
    parse_list_to_tibble()
}

#' @importFrom xml2 as_list
#' @noRd
#' @keywords Internal
parse_xml_to_list <- function(x){
  # convert xml to list
  x_list <- x |> as_list() 
  
  # 'clean' this list to be more R-like
  result <- x_list |> xml_to_list_recurse()

  # try using numeric indexes to set attributes
  # NOTE: using addresses (i.e. `names(x_list)`) doesn't work, because names can be duplicated
  index_list <- get_index(result)

  # walk along the list and assign attributes back to `clean_result`
  for(a in seq_along(index_list)){ # using purrr::walk here fails
    b <- index_list[[a]]
    z <- pluck(x_list, !!!b)
    attributes(`[[`(result, b)) <- attributes(z) # do not replace with `pluck()<-`
  }
  # return
  result
}

#' Drill into list constructed from xml to ensure terminal nodes are correct
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
xml_to_list_recurse <- function(x){
  map(.x = x,
      .f = \(a){
        if(is.list(a)){
          if(length(a) == 1){
            if(inherits(a[[1]], "character")){
              a[[1]]
            }else{
              xml_to_list_recurse(a)
            }
          }else{
            xml_to_list_recurse(a)
          }
        }else{
          a[[1]]
        }
      })
}

#' clean up the output from `index_recurse()`
#' @importFrom purrr list_flatten
#' @importFrom purrr map
#' @importFrom purrr pluck_depth
#' @noRd
#' @keywords Internal
get_index <- function(x){
  address_list <- index_recurse(x)
  # flatten lists
  n <- pluck_depth(address_list) - 1
  for(i in seq_len(n)){
    address_list <- list_flatten(address_list)
  }
  # get all unique addresses
  address_lengths <- lengths(address_list)
  n_max <- max(address_lengths)
  map(.x = seq_len(n_max), 
      .f = \(a){
        address_tmp <- address_list[address_lengths >= a]
        result <- map(address_tmp, .f = \(b){b[seq_len(a)]})
        result[!duplicated(result)]
      }) |>
    list_flatten()
}

#' drill into a list to get the 'index'; i.e. a numeric map of the list
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
index_recurse <- function(x, 
                          level = 1,
                          index_accumulate = list()){
  if(is.list(x)){
    map(.x = seq_len(length(x)),
        .f = \(a){
          index_recurse(x[[a]], 
                        level = level + 1,
                        index_accumulate = unlist(c(index_accumulate, a))
          )})    
  }else{
    index_accumulate
  }
}
# note this isn't perfect yet. Some terminal nodes still parse to `list()`