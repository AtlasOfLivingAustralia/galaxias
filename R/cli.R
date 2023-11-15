#' Helper functions for cli and logs
#' @importFrom cli col_blue bg_blue col_green col_blue
simple_h1 <- function(...) {
  tail <- 80 - nchar(paste(...))
  cat(col_blue(
    "──", paste(...),
    paste(rep("─", tail), collapse = "")
  ), "\n", sep = "")
}
simple_h2 <- function(...) {
  cat(col_blue("── ", paste(...), "\n", sep = ""))
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

print_content_with_border <- function(content_lines) {
  cat(col_blue(paste(rep("─", 80), collapse = "")), "\n", sep = "")
  for (line in content_lines) {
    cat(line, "\n", sep = "")
  }
  cat(col_blue(paste(rep("─", 80), collapse = "")), "\n", sep = "")
}

print_title_bar <- function(title) {
  cat(bg_blue(col_green(paste(rep(" ", 80), collapse = ""))), "\n", sep = "")
  cat(bg_blue(col_green(sprintf(" %-78s ", title))), "\n", sep = "")
  cat(bg_blue(col_green(paste(rep(" ", 80), collapse = ""))), "\n\n", sep = "")
}
