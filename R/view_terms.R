#' View DwC terms in the browser
#' @export
view_terms <- function() {
  shinyApp(view_terms_ui(df), 
           view_terms_server) |>
    print()  
}

view_terms_ui <- function(df){
  page_fillable(
    title = "galaxias::map_terms",
    theme = bs_theme(bootswatch = "minty"),
    h4("Darwin Core terms"),
    terms_list()
  )
}

view_terms_server <- function(input, output) {}