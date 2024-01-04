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
      col_widths = c(8, 4),
      left_col(),
      right_col()
    )
  )
}

#' right column ui
#' @importFrom htmltools div
#' @importFrom htmltools h4
#' @noRd
#' @keywords Internal
left_col <- function(){
  div(
    h4("Darwin Core terms"),
    terms_list()
  )
}

#' left column ui
#' @importFrom dplyr pull
#' @importFrom htmltools br
#' @importFrom htmltools h4
#' @importFrom purrr pluck
#' @importFrom shiny actionButton
#' @noRd
#' @keywords Internal
right_col <- function(){
  div(
    h4("Supplied terms"),
    colnames_div(df),
    detect_colnames(df),
    br(),
    h4("tracking"),
    # verbatimTextOutput("x"), # testing only
    actionButton(
      inputId = "exit",
      label = "Save & Exit"
    ),
    move_term_to_colname(group = {get_terms() |> 
        pluck("parents") |> 
        pull("code")}),
    remove_terms_from_list(df)
  )
}

#' server function for map_terms()
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactiveValues
#' @importFrom shiny stopApp
#' @importFrom stringr str_extract
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
  )

  # as changes are made, update data storage with user mappings
  observe({
    lapply(
      seq_len(ncol(df)),
      function(a){
        check_input <- input[[paste0("sort_", a)]] |>
          str_extract("^[:alpha:]+\\n") |>
          sub("\\n$", "", x = _)
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
  # output$x <- renderPrint({str(mapping$result)})
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

#' build a list of terms with navbar
#' @importFrom bslib navset_pill_list
#' @importFrom bslib nav_panel
#' @importFrom dplyr filter
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
terms_list <- function(){
  navset_pill_list(
    !!!map(
      .x = get_terms()$parents$code,
      .f = \(x){
        nav_panel(
          title = x,
          terms_div(
            terms = {get_terms() |>
                pluck("terms") |>
                filter(parent_class == x)},
            group = x)
        )
      }
    ),
    well = FALSE
  )
}

#' Internal function to convert terms to boxes
#' @importFrom htmltools div
#' @importFrom purrr map
#' @importFrom shiny markdown
#' @noRd
#' @keywords Internal
terms_div <- function(terms, group){
  div(
    class = "panel-body",
    id = paste0("sort_terms_", group),
    map(.x = seq_len(nrow(terms)),
           .f = \(x){
             custom_value_box(
               term = terms$code[x],
               markdown(paste0("*", terms$description[x], "*")),
               markdown(paste0("**Example:** ", 
                               strsplit(terms$examples[x], ";")[[1]][1]
               )),
               theme = "success"
             )
             # card(
             #      class = "terms",
             #      card_body(terms$code[x], class = "value-box-title"),
             #      shiny::markdown(terms$description[x])
             #      # title = terms$description[x],
             #      # value = terms$code[x]
             #      # card_footer(terms$examples[x]))
             # ) 
           })
  )
}

#' Internal function to build own boxes - heavily based on `value_box()` code
#' @importFrom bslib card
#' @importFrom bslib as_fill_carrier
#' @importFrom bslib value_box_theme
#' @importFrom htmltools css
#' @importFrom htmltools div
#' @importFrom htmltools p
#' @noRd
#' @keywords Internal
custom_value_box <- function(term, ..., theme){
  dots <- list(...)
  term <- p(term, class = "value-box-value")
  contents <- div(class = "value-box-area", term, !!!dots) |>
    as_fill_carrier()
  theme <- value_box_theme(theme)
  card(class = c("bslib-value-box", theme$class),
       style = css(color = theme$fg, 
                   background_color = theme$bg, 
                   `--bslib-color-fg` = theme$fg, 
                   `--bslib-color-bg` = theme$bg),
       contents)
}

#' Internal function to quantify which terms are added to each colname
#' @importFrom purrr map
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
#' @importFrom purrr map
#' @importFrom sortable sortable_js
#' @importFrom sortable sortable_options
#' @noRd
#' @keywords Internal
move_term_to_colname <- function(group){
  map(.x = group,
      .f = \(x){
        sortable_js(
          css_id = paste0("sort_terms_", x),
          options = sortable_options(
            group = list(
              name = "sortGroup1",
              put = TRUE
            ),
            sort = FALSE
            # onSort = sortable_js_capture_input("terms_remaining")
          )
        )
      })
}

#' Support dropping divs into a new table
#' @importFrom htmlwidgets JS
#' @importFrom purrr map
#' @importFrom sortable sortable_js
#' @importFrom sortable sortable_options
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