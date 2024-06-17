#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom snakecase to_lower_camel_case
#' @noRd
#' @keywords Internal
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
    arrange(.data$start_row) |>
    mutate(label = to_lower_camel_case(.data$label)) |>
    get_md_text(x) |>
    select("level", "label", "attributes", "text")
}

#' @noRd
#' @keywords Internal
parse_md_to_list <- function(x){
  x |>
    parse_md_to_tibble() |>
    parse_tibble_to_list()
}

#' @noRd
#' @keywords Internal
parse_md_to_xml <- function(x){
  x |>
    parse_md_to_tibble() |>
    parse_tibble_to_list() |>
    parse_list_to_xml()
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