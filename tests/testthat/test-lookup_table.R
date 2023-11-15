# Test that split_into_words works
# Test that word overlap detects matches correctly
# Easy example
data <- occurrence_data
data2 <- occurrence_data
col <- colnames(data)[3]
col2 <- "camelLanguage"
not <- "organism"
words1 <- split_into_words(col2)
words2 <- split_into_words(dwc_terms_archived$column_name[50])
length(intersect(words1, words2))

# this is a good string to test on because it gives 2 matches for a dwc term and
# 1 for another dwc term. so you can test filtering and ordering of matches etc.
col <- "infraspecific epithet"

test_mapping <- map_fields(occurrence_data)



cli_ul(cli::cli_dl(c("column" = col, index = which(colnames(data) == col))))

cat(create_border_line(), "\n")
cat(create_bullet_list(suggestions), "\n")

create_border_line <- function() {
  return(paste(rep("─", 80), collapse = ""))
}

create_bullet_list <- function(items) {
  bullet_list <- ""
  for (i in seq_along(items)) {
    bullet_list <- paste0(
      bullet_list,
      cli::col_blue(sprintf(
        " • %s\n",
        items[i]
      ))
    )
  }
  return(bullet_list)
}

# CLI stuff
char_vector <- c("hello", "world", "juiced")
cli_ul("{.field {char_vector}}")
cli_ul("{.var {char_vector}}")
cli_ul("{.val {char_vector}}")
cli_ul(paste0("{.var .strong ", char_vector, "}"))


# Function to print a title bar with the correct crayon functions
print_title_bar <- function(title) {
  cat(bgBlue(green(paste(rep(" ", 80), collapse = ""))), "\n", sep = "")
  cat(bgBlue(green(sprintf(" %-78s ", title))), "\n", sep = "")
  cat(bgBlue(green(paste(rep(" ", 80), collapse = ""))), "\n\n", sep = "")
}

# Function to print a menu or key bindings box with the correct crayon functions
print_menu_box <- function(menu_items) {
  cat(bgWhite(blue(paste(rep(" ", 30), collapse = ""))), "\n", sep = "")
  for (item in menu_items) {
    cat(bgWhite(blue(sprintf(" %-28s ", item))), "\n", sep = "")
  }
  cat(bgWhite(blue(paste(rep(" ", 30), collapse = ""))), "\n", sep = "")
}

# Function to print content with a border with the correct crayon functions
print_content_with_border <- function(content_lines) {
  cat(blue(paste(rep("─", 80), collapse = "")), "\n", sep = "")
  for (line in content_lines) {
    cat(line, "\n", sep = "")
  }
  cat(blue(paste(rep("─", 80), collapse = "")), "\n", sep = "")
}

# Example usage with the corrected functions
print_title_bar("New Debian Project Leader elected - Neil McGovern")
menu_items <- c(
  "UP/DOWN or 'j/k' : Scroll to the prev/next item",
  "'ENTER' or 'o'   : Open the selected item in the default web browser",
  "More menu items..."
)

print_menu_box(menu_items)

content_lines <- c(
  "hnrnbrot : 120 pts 12hr",
  "Well, he's got the right name for it,",
  "More content lines..."
)

print_content_with_border(content_lines)
simple_h1("hello")
simple_h1 <- function(...) {
  cat(blue(
    "──", paste(...),
    paste(rep("─", tail), collapse = "")
  ), "\n", sep = "")
}
simple_h2 <- function(...) {
  cat(blue("── ", paste(...), "\n", sep = ""))
}

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

# Example usage
instructions <- c("Number (>0)", "0", "Type 'exit'")
definitions <- c(
  "Map a suggested term",
  "No matches, continue to manual input",
  "Exit early (session mappings will be preserved)"
)

# Create the prompt text
prompt_text <- create_aligned_prompt(instructions, definitions)

# Print the prompt text up to the user input line
cat(prompt_text)
# Capture the user input
user_input <- readline(prompt = "Please enter your choice: ")
cat(cli::col_blue(
  "──", paste("hello"),
  paste(rep("─", 6), collapse = "")
), "\n", sep = "")

# make simple data frame
azz <- data.frame(
  a = c(1, 2, 3),
  b = c(4, 5, 6)
)
azz[1, 1] <- 5

az2 <- tibble::tibble(azz)
az2[1, 1] <- 5
hello <- az2[1, 1]
hello <- azz[1, 1]
str(hello)
str(hello)
class(hello)
