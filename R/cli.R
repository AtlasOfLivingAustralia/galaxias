# Helper functions for cli and logs

#' Simple level 1 header
#' @param ... character vector of text to print
#' @return NULL
#' @noRd
#' @keywords internal
#' @importFrom cli bg_blue bg_white col_green col_blue
simple_h1 <- function(...) {
  tail <- 80 - nchar(paste(...))
  cat(col_blue(
    "──", paste(...),
    paste(rep("─", tail), collapse = "")
  ), "\n", sep = "")
}
#' Simple level 2 header
#' @param ... character vector of text to print
#' @return NULL
#' @noRd
#' @keywords internal
simple_h2 <- function(...) {
  cat(col_blue("── ", paste(...), "\n", sep = ""))
}

#' Create aligned text prompt for cli
#' Similar to the definition list in cli, except this aligns text for both left
#' and right hand sides adds borders.
#
#' @param instruction character vector for left hand side
#' @param definition character vector for right hand side
#' @return NULL
#' @noRd
#' @keywords internal
#' @examples
#' instructions <- c("Number (>0)", "0", "Type 'exit'")
#' definitions <- c(
#'   "Map a suggested term",
#'   "No matches, continue to manual input",
#'   "Exit early (session mappings will be preserved)"
#' )
#' prompt_text <- create_aligned_prompt(instructions, definitions)
#' cat(prompt_text)
create_aligned_prompt <- function(instruction, definition) {
  instruction <- as.character(instruction)
  definition <- as.character(definition)
  combined <- mapply(function(instr, def) {
    paste(instr, "::", def)
  }, instruction, definition, SIMPLIFY = FALSE)

  max_length <- max(nchar(sub(" ::.*", "", combined)))

  aligned_instruction <- sapply(combined, function(instr) {
    parts <- strsplit(instr, " ::")[[1]]
    sprintf("%-*s :: %s", max_length, parts[1], parts[2])
  }, USE.NAMES = FALSE)

  border_length <- max(nchar(aligned_instruction))
  border <- paste(rep("─", border_length), collapse = "")

  paste(border, "\n",
    paste(aligned_instruction, collapse = "\n"), "\n",
    border, "\n",
    sep = ""
  )
}

print_content_with_border <- function(content_lines) {
  cat(col_blue(paste(rep("─", 80), collapse = "")), "\n", sep = "")
  for (line in content_lines) {
    cat(line, "\n", sep = "")
  }
  cat(col_blue(paste(rep("─", 80), collapse = "")), "\n", sep = "")
}

#' Print a title bar for cli
#' @param title character vector of title
#' @return NULL
#' @noRd
#' @keywords internal
#' @examples
#' print_title_bar("I like turtles")
print_title_bar <- function(title) {
  cat(bg_blue(col_green(paste(rep(" ", 80), collapse = ""))), "\n", sep = "")
  cat(bg_blue(col_green(sprintf(" %-78s ", title))), "\n", sep = "")
  cat(bg_blue(col_green(paste(rep(" ", 80), collapse = ""))), "\n\n", sep = "")
}

#' Print menu box for cli
#' @param menu_items character vector of menu items
#' @return NULL
#' @noRd
#' @keywords internal
#' @examples
#' menu_items <- c(
#'   "UP/DOWN or 'j/k' : Scroll to the prev/next turtle",
#'   "'ENTER' or 'o'   : Open the selected turtle in the default web habitat",
#'   "More menu items..."
#' )
#' print_menu_box(menu_items)
print_menu_box <- function(menu_items) {
  cat(cli::bg_white(cli::col_blue(paste(rep(" ", 30), collapse = ""))),
    "\n",
    sep = ""
  )
  for (item in menu_items) {
    cat(cli::bg_white(cli::col_blue(sprintf(" %-28s ", item))),
      "\n",
      sep = ""
    )
  }
  cat(cli::bg_white(cli::col_blue(paste(rep(" ", 30), collapse = ""))),
    "\n",
    sep = ""
  )
}
