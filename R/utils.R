## -- {cli} functions -- ##

#' Interactive menu function
#' @description
#' Built on top of utils::menu(). 
#' Originally proposed by Hadley here: https://github.com/r-lib/cli/issues/228#issuecomment-1453614104
#' Full code from gargle here: https://github.com/r-lib/gargle/blob/main/R/utils-ui.R
#' @noRd
#' @keywords Internal
cli_menu <- function(header,
                     prompt,
                     choices,
                     not_interactive = choices,
                     exit = integer(),
                     .envir = rlang::caller_env(),
                     error_call = rlang::caller_env()) {
  if (!rlang::is_interactive()) {
    cli::cli_abort(
      c(header, not_interactive),
      .envir = .envir,
      call = error_call
    )
  }
  
  choices <- paste0(cli::style_bold(seq_along(choices)), ": ", choices)
  cli::cli_inform(
    c(header, prompt, choices),
    .envir = .envir
  )
  
  repeat {
    selected <- cli_readline("Selection: ")
    if (selected %in% c("0", seq_along(choices))) {
      break
    }
    cli::cli_inform(
      "Enter a number between 1 and {length(choices)}, or enter 0 to exit."
    )
  }
  
  selected <- as.integer(selected)
  if (selected %in% c(0, exit)) {
    if (is_testing()) {
      cli::cli_abort("Exiting...", call = NULL)
    } else {
      cli::cli_alert_danger("Exiting...")
      # simulate user pressing Ctrl + C
      invokeRestart("abort")
    }
  }
  
  selected
}

#' Interactive readLines
#' @description
#' Allows for interactive testing of `cli_menu()` selection. 
#' Originally proposed by Hadley here: https://github.com/r-lib/cli/issues/228#issuecomment-1453614104.
#' Full code from gargle here: https://github.com/r-lib/gargle/blob/main/R/utils-ui.R
#' @noRd
#' @keywords Internal
cli_readline <- function(prompt) {
  local_input <- getOption("cli_input", character())
  
  # not convinced that we need to plan for multiple mocked inputs, but leaving
  # this feature in for now
  if (length(local_input) > 0) {
    input <- local_input[[1]]
    cli::cli_inform(paste0(prompt, input))
    options(cli_input = local_input[-1])
    input
  } else {
    readline(prompt)
  }
}


## -- testing -- ##

#' Mimic supplying user input to a menu
#' @noRd
#' @keywords Internal
local_user_input <- function(x, env = rlang::caller_env()) {
  withr::local_options(
    rlang_interactive = TRUE,
    # trailing 0 prevents infinite loop if x only contains invalid choices
    cli_input = c(x, "0"),
    .local_envir = env
  )
}

#' Check whether function is being called by testthat
#' @noRd
#' @keywords Internal
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


## -- checks -- ##

#' Check an argument path in `galaxias_config()`
#' @description
#' Checks that argument is 
#'   1. not null 
#'   2. a character 
#'   3. exists (optional)
#' @noRd
#' @keywords Internal
check_config_path <- function(...,
                              must_exist = TRUE,
                              error_call = rlang::caller_env()){
  
  
  is_name <- vapply((dots(...)), is.symbol, logical(1))
  if (!all(is_name)) {
    cli::cli_abort("Can only use existing named objects.",
                   call = error_call)
  }
  
  # save name
  obj_name <- dots(...)[[1]]
  
  if(is.null(obj_name)){
    cli::cli_abort(c("Missing path or filename, with no default."),
                   call = error_call)
  }
  
  # save obj contents (probably containing a path to a file or folder)
  path <- rlang::eval_tidy(...)
  
  if(!inherits(path, "character")){
    class_file <- class(path)
    cli::cli_abort(c("{.arg {obj_name}} must be of class character, not {class_file}.",
                     i = "See {.code ?galaxias_config()}."),
                   call = error_call)
  }
  
  if(must_exist){
    if(!file.exists(path)){
      c("Specified {.arg {obj_name}} does not exist.",
        x = "Can't find {.file {path}}.",
        i = "See {.code ?galaxias_config()}.") |>
        cli::cli_abort(call = error_call)
    }
  }
}


#' Check whether publish directory exists, and if not, build it,
#' depending on quiet and interactive
#' @noRd
#' @keywords Internal
check_publish_directory <- function(quiet,
                                    error_call = rlang::caller_env()){
  directory <- potions::pour("directory",
                             .pkg = "galaxias")
  if(!file.exists(directory)){
    if(rlang::is_interactive() & !quiet){ 
      
      choice <- cli_menu(
        c(" ",
          "Your working directory for data publication is set to {.file {directory}}, which does not exist.", 
          " "),
        "Would you like to create it? (0 to exit)",
        choices = c("Yes", "No")
      )
      
      if (choice == 1) {
        usethis::use_directory(directory)
      } else {
        cli::cli_abort(c("Unable to build working directory.", 
                         i = "To change the default directory, see `galaxias_config()`."),
                       call = error_call)
      }
    }else{
      usethis::use_directory(directory)
    } 
  }
  directory
}