#' Parse between common object types
#' 
#' It is important in `galaxias` to be able to move between `tibble`s, `list`s, 
#' and `xml` documents. Primarily called by `read_md()` and `write_md()`, these 
#' functions are exported for clarity and debugging purposes.
#'
#' In practice, the sequence of transformations is fixed, being `string` >
#' `tibble` > `list` > `xml`. Nonetheless, it can be useful sometimes to have 
#' helper functions that handle this hierarchy 'under the hood'.
#'
#' Note that `parse_tibble_to_list()` is a recursive function, and is highly
#' bespoke to tibbles of the type returned by `parse_md_to_tibble()`. 
#' `parse_list_to_xml()` is synonymous with `xml2::as_xml_document()` and
#' `parse_xml_to_list()` is synonymous with `xml2::as_list()`
#' @param x For markdown parsers, a vector of class `character`, such as 
#' imported using `base::readLines()`, containing text in markdown format. For
#' other functions, an object of class `list` or `xml_document`.
#' @name parse
#' @order 1
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @export
parse_md_to_tibble <- function(x){
  
  # find titles, in either `<h1>` or `#` format
  title_check <- grepl("^\\s*(#|<h[[:digit:]])", x)
  
  # get html headers first
  equals_check <- grepl("=", x) & title_check # note: can only happen with `<hn>` structure
  # also note: we subset after checking so the index is to x, not x[title_check]
  # code for seeking end of header in hn format
  tibble1 <- get_header_label_html(x, which(equals_check))
  
  # then markdown headers
  markdown_check <- grepl("^#+", x) & title_check
  tibble2 <- get_header_label_md(x, which(markdown_check))
  
  # join and order
  bind_rows(tibble1, tibble2) |>
    arrange(start_row) |>
    mutate(label = to_lower_camel_case(label)) |>
    get_md_text(x) |>
    select("level", "label", "attributes", "text")
}

#' @rdname parse
#' @order 2
#' @export
parse_md_to_list <- function(x){
  parse_md_to_tibble(x) |>
    tibble_to_list()
}

#' @rdname parse
#' @order 3
#' @importFrom xml2 as_xml_document
#' @export
parse_md_to_xml <- function(x){
  parse_md_to_list(x) |>
    as_xml_document()
}

## remaining to code:
# parse_tibble_to_md()
# parse_list_to_md
# parse_xml_to_md
## These can probably be amended from code in `write_md.R`

#' @rdname parse
#' @order 7
#' @param df A `tibble` built within `md_to_tibble()`
#' @export
parse_tibble_to_list <- function(df){
  xml_recurse(df, level = 1)
}

#' Internal function to power `parse_tibble_to_list()`
#' necessary to prevent problems if user sets `level` arg
#' @noRd
#' @keywords Internal
xml_recurse <- function(x, level = 1){
  if(nrow(x) == 1){
    list(x$content)
  }else{
    this_level <- x$depth == level
    x_list <- split(x, cumsum(this_level))
    if(level > 1){
      x_list <- x_list[-1]
    }
    names(x_list) <- x$name[this_level]
    lapply(x_list, function(a){xml_recurse(a, level = level + 1)})    
  }
}

#' @rdname parse
#' @order 8
#' @importFrom xml2 as_xml_document
#' @export
parse_tibble_to_xml <- function(df){
  parse_tibble_to_list(df) |>
    as_xml_document()
}

#' @rdname parse
#' @order 9
#' @importFrom xml2 as_xml_document
#' @export
parse_list_to_xml <- function(x){
  as_xml_document(x)
}

#' @rdname parse
#' @order 10
#' @importFrom xml2 as_list
#' @export
parse_xml_to_list <- function(x){
  as_list(x)
}

#' get header attributes when formatted as ##Header
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
get_header_label_md <- function(string, rows){
  if(length(rows) < 1){
    NULL
  }else{
    tibble(
      start_row = rows,
      end_row = rows, 
      level = {regexpr("^#{1,}", string[rows]) |>
          attr("match.length")},
      label = str_remove(string[rows], "^#+\\s*")
    )    
  }
}

#' get header attributes when formatted as <h1> etc
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
get_header_label_html <- function(string, rows){
  if(length(rows) < 1){
    NULL
  }else{
    map(.x = seq_along(rows),
        .f = function(i){extract_header_html(i, rows, string)}) |>
      bind_rows()    
  }
}


#' Internal function to parse each section as xml
#' @importFrom glue glue_collapse
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_text
#' @noRd
#' @keywords Internal
extract_header_html <- function(i, rows, string){
  xx <- rows[i]
  # work out header level
  level <- str_extract(string[xx], "<h[[:digit:]]{1}") |>
    str_remove("<")
  # get closing row
  which_close <- grepl(glue("</{level}>"), string) |>
    which()
  # get vector of rows to collapse together
  row_close <- min(which_close[which_close > xx])
  lookup <- seq(
    from = xx,
    to = row_close)
  temp_string <- glue_collapse(string[lookup])
  # use xml to parse string
  result_xml <- read_xml(temp_string)
  tibble(
    start_row = xx,
    end_row = row_close,
    level = as.integer(str_extract(level, "[:digit:]")),
    label = xml_text(result_xml),
    attributes = as.list(xml_attrs(result_xml)))
}


#' Internal function to find text rows and assign them correctly
#' TODO: Support internal paragraph breaks
#' @importFrom glue glue_collapse
#' @importFrom purrr map
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
get_md_text <- function(df, string){
  # n_rows <- nrow(df)
  text_df <- tibble(
    start = df$end_row + 1,
    end = c(df$start_row[-1] - 1, length(string))) |>
    mutate(row_length = end - start)
  
  text <- map(  
    .x = split(text_df, seq_len(nrow(text_df))),
    .f = \(i){
      if(i$row_length < 0){
        NA
      }else if(i$row_length == 0){
        string[i$start]
      }else{
        glue_collapse(string[seq(i$start, i$end)], sep = " ")
      }
    }) |>
    unlist() |>
    trimws() |>
    str_replace("\\s{2,}", "\\s")
  
  df$text <- text
  df
}