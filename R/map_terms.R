#' Shiny app for mapping terms
#' 
#' Details to follow
#' @importFrom shiny shinyApp
#' @export
map_terms <- function(df){
  if(missing(df)){
    stop("'data' argument is missing, with no default")}
  if(!inherits(df, "data.frame")){
    stop("supplied object must inherit from class 'data.frame'")} # swap to `dwca`
  shinyApp(map_terms_ui(df), 
           map_terms_server)
}

#' ui function for map_terms()
#' @importFrom bslib bs_theme
#' @importFrom htmltools br
#' @importFrom htmltools h4
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidPage
#' @importFrom shiny fluidRow
#' @importFrom shiny titlePanel
#' @noRd
#' @keywords Internal
map_terms_ui <- function(df){
  fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    titlePanel(title = "galaxias::map_terms"),
    fluidRow(
      column(
        width = 6,
        h4("Supplied column names"),
        colnames_div(df),
        detect_colnames(df),
        br(),
        h4("tracking"),
        # testing only #
        verbatimTextOutput("sort_1"),
        verbatimTextOutput("sort_2"),
        verbatimTextOutput("sort_3")
        # end testing #
      ),
      column(
        width = 6,
        h4("Occurrence"),
        terms_div(),
        move_term_to_colname(),
        remove_terms_from_list(colnames(df))
      )
    )
  )
}

#' server function for map_terms()
#' @importFrom shiny renderPrint
#' @noRd
#' @keywords Internal
map_terms_server <- function(input, output) {
  # ## try to capture where these terms are put
  output$sort_1 <- renderPrint(input$sort_1)
  output$sort_2 <- renderPrint(input$sort_2)
  output$sort_3 <- renderPrint(input$sort_3)
}

#' Internal function to add 
#' @importFrom htmltools div
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
colnames_div <- function(df){
  cols <- colnames(df)
  map(.x = seq_len(ncol(df)),
      .f = \(id){
        div(
          class = "panel panel-default",
          div(class = "panel-heading", cols[id]),
          div(
            class = "panel-body",
            id = paste0("x", id)
          )
        )
      }
    )
}

#' Internal function to convert terms to divs
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom htmltools div
#' @importFrom htmltools tag
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
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
      id = "sort_terms",
      map(.x = terms,
          .f = \(x){tag("button", list(class = "terms", x))})
    )
  )
}

#' Internal function to quantify which terms are added to each colname
#' @importFrom sortable sortable_js
#' @importFrom sortable sortable_js_capture_input
#' @importFrom sortable sortable_options
#' @noRd
#' @keywords Internal
detect_colnames <- function(df){
  map(.x = seq_len(ncol(df)),
      .f = \(id){
        sortable_js(
          css_id = paste0("x", id),
          options = sortable_options(
            group = list(name = "sortGroup1"),
            onAdd = sortable_js_capture_input(paste0("sort_", id)),
            onRemove = sortable_js_capture_input(paste0("sort_", id))
          )
        )
      })
}

#' JS support for draggable elements
#' @importFrom sortable sortable_js
#' @importFrom sortable sortable_options
#' @noRd
#' @keywords Internal
move_term_to_colname <- function(){
  sortable_js(
    css_id = "sort_terms",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      sort = FALSE
      # onSort = sortable_js_capture_input("terms_remaining")
    )
  )
}

#' Support dropping divs into a new table
#' @importFrom htmlwidgets JS
#' @importFrom sortable sortable_js
#' @importFrom sortable sortable_options
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
remove_terms_from_list <- function(id){
  map(
    .x = id,
    .f = \(x){
      sortable_js(
        css_id = paste0("sort_", x),
        options = sortable_options(
          group = list(
            name = "sortGroup1",
            put = JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          )
          # onSort = sortable_js_capture_input("terms_moved")
        )
      )
    }
  )
}