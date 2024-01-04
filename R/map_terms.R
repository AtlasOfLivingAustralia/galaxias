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
           map_terms_server) |>
    print()
}

#' ui function for map_terms()
#' @importFrom bslib bs_theme
#' @importFrom bslib layout_columns
#' @importFrom bslib page_fillable
#' @noRd
#' @keywords Internal
map_terms_ui <- function(df){
  page_fillable(
    title = "galaxias::map_terms",
    theme = bs_theme(bootswatch = "minty"),
    layout_columns(
      widths = c(6, 6),
      left_col(),
      right_col()
    )
  )
}

#' left column ui
#' @importFrom htmltools br
#' @importFrom htmltools h4
#' @importFrom shiny actionButton
#' @importFrom shiny verbatimTextOutput
#' @noRd
#' @keywords Internal
left_col <- function(){
  div(
    h4("Supplied column names"),
    colnames_div(df),
    detect_colnames(df),
    br(),
    h4("tracking"),
    verbatimTextOutput("x"), # testing only
    actionButton(
      inputId = "exit",
      label = "Save & Exit"
    )
  )
}

#' right column ui
#' @importFrom htmltools br
#' @importFrom htmltools h4
#' @importFrom shiny textOutput
#' @importFrom shiny uiOutput
#' @noRd
#' @keywords Internal
right_col <- function(){
  div(
    h4("Darwin Core terms"),
    # selectInput(
    #   inputId = "select_class",
    #   label = "Choose Darwin Core class",
    #   choices = get_terms()$parents$code,
    #   selected = "Occurrence"
    # ),
    # textOutput("current_description"),
    br(),
    terms_div({get_terms() |>
        pluck("terms") |>
        filter(parent_class == "Occurrence")}),
    # uiOutput("current_terms"), # seems to break stuff
    move_term_to_colname(),
    remove_terms_from_list(df)
  )
}

#' server function for map_terms()
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr pluck
#' @importFrom shiny renderPrint
#' @importFrom shiny selectInput
#' @importFrom shiny stopApp
#' @noRd
#' @keywords Internal
map_terms_server <- function(input, output) {
  
  # create an updatable object to store column name mappings
  mapping <- reactiveValues(
    result = {
      x <- rep(NA, ncol(df)) |>
        as.list()
      names(x) <- colnames(df)
      x
    }
    # visible_terms = {
    #   get_terms() |>
    #     pluck("terms") |>
    #     filter(parent_class == "Occurrence")
    # },
    # class_text = {
    #   get_terms() |>
    #     pluck("parents") |>
    #     filter(code == "Occurrence") |>
    #     pull("description")
    # }
  )
  
  ## This approach - updating displayed fields via `selectInput` - 
  ## appears to break drag-and-drop for unknown reasons
  # # track the selector for DwC classes
  # observeEvent(input$select_class, {
  #   mapping$visible_terms <- get_terms() |>
  #     pluck("terms") |>
  #     filter(parent_class == input$select_class)
  #   mapping$class_text <- get_terms() |>
  #     pluck("parents") |>
  #     filter(code == input$select_class) |>
  #     pull("description")
  # })
  # 
  # # update class text to give a description
  # output$current_description <- renderText({
  #   mapping$class_text
  # })
  # 
  # # update UI to show only selected terms
  # output$current_terms <- renderUI({
  #   terms_div(mapping$visible_terms)
  # })

  # as changes are made, update data storage with user mappings
  observe({
    lapply(
      seq_len(ncol(df)),
      function(a){
        check_input <- input[[paste0("sort_", a)]]
        if(!is.null(check_input)){
          if(length(check_input) < 1){
            mapping$result[[a]] <- NA  
          }else{
            mapping$result[[a]] <- check_input    
          }
        }else{
          mapping$result[[a]] <- NA
        }
      }
    )
  })
  
  # when requested, save mappings back to the workspace
  observeEvent(input$exit, {
    stopApp(returnValue = mapping$result) # note: should instead update a `dwca` object
  })

  ## for testing purposes only, track internal objects in the browser
  output$x <- renderPrint({str(mapping$result)})
}

#' Internal function to add 
#' @importFrom bslib card
#' @importFrom bslib card_body
#' @importFrom bslib card_footer
#' @importFrom bslib card_header
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
colnames_div <- function(df){
  cols <- colnames(df)
  map(.x = seq_len(ncol(df)),
      .f = \(id){
        card(
          card_header(cols[id]), # left-align?
          card_body(id = paste0("x", id)),
          card_footer(paste0(class(df[[id]]), ": ", df[[id]][1]))
        )
      }
    )
}

#' Internal function to convert terms to divs
#' @importFrom bslib card
#' @importFrom bslib card_body
#' @importFrom bslib card_footer
#' @importFrom bslib card_header
#' @importFrom dplyr filter
#' @importFrom htmltools div
#' @importFrom htmltools tag
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
terms_div <- function(terms){
  div(
    class = "panel-body",
    id = "sort_terms",
    map(.x = seq_len(nrow(terms)),
           .f = \(x){
             # tag("button", list(class = "terms", x)) # works
             card(class = "terms",
                  card_header(terms$code[x]),
                  card_body(terms$description[x]))
                  # card_footer(terms$example[x]) # not yet available
           })
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
remove_terms_from_list <- function(df){
  map(
    .x = seq_len(ncol(df)),
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