#' Shiny app for mapping terms
#' 
#' Details to follow
#' @importFrom shiny shinyApp
#' @export
map_terms <- function(df){
  
  # basic type checks
  if(missing(df)){
    stop("'data' argument is missing, with no default")}
  if(!inherits(df, "data.frame")){
    stop("supplied object must inherit from class 'data.frame'")}
  
  ui <- ala_darwinise_ui(df)
  
  # NOTE: server is required, but doesn't do anything yet
  # needs some code here to actually build the DwC archive when triggered
  server <- function(input, output) {}
  
  shinyApp(ui, server)
}

#' get terms and format them
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tawnydragon tdwg
#' @importFrom tawnydragon tdwg_standards
#' @importFrom tawnydragon tdwg_terms
#' @importFrom tawnydragon summarize
#' @noRd
#' @keywords Internal
get_terms <- function(){
  terms <- tdwg() |>
    tdwg_standards(label == "Darwin Core",
                   status == "recommended") |>
    tdwg_terms() |>
    summarize()
  
  parents <- terms |>
    filter(is.na(parent_class) & 
             code %in% parent_class) |>
    select(-parent_class, -status)
  
  terms <- terms |>
    filter(!is.na(parent_class))  
  
  list(parents = parents, terms = terms)
}

#' function to build a user interface for DwC allocation
#' @importFrom bslib bs_theme
#' @importFrom htmltools h3
#' @importFrom htmltools div
#' @importFrom shiny actionButton
#' @importFrom shiny fluidPage
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny titlePanel
#' @noRd
#' @keywords Internal
ala_darwinise_ui <- function(df){
  fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    titlePanel(title = "galaxias::map_terms"),
    fluidRow(
      column(
        width = 6,
        add_colnames_table(df)
      ),
      column(
        width = 6,
        h4("Occurrence"),
        terms_div(),
        enable_dragging(),
        add_droppable_labels(colnames(df))
      )
    )
  )
}

#' Internal function to convert terms to divs
#' @noRd
#' @keywords Internal
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr pluck
terms_div <- function(){
  terms <- get_terms() |>
    pluck("terms") |>
    filter(parent_class == "Occurrence") |>
    pull(code)
  div(
    class = "panel panel-default",
    div(class = "panel-heading", "DwC Terms"),
    div(
      class = "panel-body",
      id = "sort1",
      map(.x = terms,
          .f = \(x){tag("button", list(class = "terms", x))})
    )
  )
}

#' function to add table of colnames with dragging option
#' @importFrom htmltools div
#' @importFrom htmltools tag
#' @importFrom htmltools tagList
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
add_colnames_table <- function(df){
  tag("table",
      tagList(
        tag("tr",
            tagList(
              tag("th", "Provided term"),
              tag("th", "DwC term")
            )
        ),
        map(.x = colnames(df),
            .f = \(x){
              tag("tr", tagList(
                tag("td", x),
                tag("td", tagList(div(id = x)))
              ))
            })
      )
  )
}

#' JS support for draggable elements
#' @importFrom sortable sortable_js
#' @importFrom sortable sortable_js_capture_input
#' @importFrom sortable sortable_options
#' @noRd
#' @keywords Internal
enable_dragging <- function(){
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars")
    )
  )
}

#' Support dropping divs into a new table
#' @noRd
#' @keywords Internal
add_droppable_labels <- function(id){
  map(
    .x = id,
    .f = \(x){
      sortable_js(
        x,
        options = sortable_options(
          group = list(
            group = "sortGroup1",
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable_js_capture_input("sort_x")
        )
      )
    }
  )
}