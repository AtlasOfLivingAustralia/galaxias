#' Check occurrence data for Darwin Core conformance
#' 
#' Function to check whether a `data.frame` or `tibble` conforms to Darwin 
#' Core standards. While most users will only want to call `suggest_workflow()`,
#' the underlying check functions are exported for detailed work, or for 
#' debugging.
#' @param .df A tibble against which checks should be run
#' @importFrom rlang inform
#' @importFrom purrr map
#' @importFrom cli cli_bullets
#' @importFrom cli col_magenta
#' @importFrom cli col_red
#' @returns Invisibly returns the input, but primarily called for the 
#' side-effect of running check functions on that input.
#' @order 1
#' @export
check_occurrences <- function(.df){
  
  # dwc_terms
  fields <- colnames(.df)
  available_checks <- fn_to_term_table() |>
    bind_rows() |>
    select(dwc_term) |>
    pull()
  checkable_fields <- fields[fields %in% available_checks]
  
  # find fields in .df with available checks
  check_functions_names <- c(glue("check_{checkable_fields}"))
  check_functions <- as.list(check_functions_names)
  names(check_functions) <- checkable_fields
  
  # print table headers
  headers <- paste0(
    ansi_align(" ", max(ansi_nchar(symbol$tick))), " ",
    ansi_align(col_blue("Column"), max(ansi_nchar(checkable_fields))), " ",
    ansi_align(col_red("E"), max(ansi_nchar("E "))), " ",
    ansi_align(col_green("P"), max(ansi_nchar("P")))
  )
  cat_line(headers)
  invisible() # prevent df results from printing with headers
  
  # Check all checkable fields, save fields & error messages
  check_results <- 
    check_functions_names |>
    purrr::map(~ check_all(.x, .df, checkable_fields)) |>
    bind_rows()
  
  # result summary
  summary_message(check_results, checkable_fields)
  
  # truncate to 5 messages if there are more than 5
  if(length(check_results$messages) > 5) {
    check_results <- check_results |>
      slice_head(n = 5)
    
    cli::cli_h3(col_blue("Truncating to first 5 error messages"))
  }
  
  # split messages by function for message formatting
  results_split <- check_results |>
    unnest(messages) |>
    mutate(
      term = factor(term, levels = unique(term)) # maintain original term order
      ) |>
    dplyr::group_split(term) 
  
  # print preserved errors in a nice format
  results_split |>
    map(~ format_messages_from_checks(.x))
  
  
  invisible(.df)
}




#' Check all fields that match Darwin Core terms in a dataframe
#' 
#' @description
#' Runs checks on all columns that match Darwin Core terms. `check_all()` does this by 
#' detecting and matching matched Darwin Core terms to their associated `check_` function. 
#' 
#' `check_all()` runs in a similar way to `devtools::test()`, whereby it will run and 
#' report the results of checks "live". `check_all()` will then return a summary table and 
#' any error messages returned by galaxias.
#' 
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_update
#' @importFrom cli ansi_align
#' @importFrom cli ansi_nchar
#' @importFrom stringr str_remove_all
#' @importFrom rlang cnd_muffle
#' @noRd
#' @keywords Internal
check_all <- function(fn, .df, checkable_fields) {
  
  # message saving & counting setup
  m_counter <- 0
  msgs <- list()
  passing <- list()
  all_results <- list()
  
  # message format setup
  field_nchar <- max(ansi_nchar(checkable_fields))
  fn_name <- stringr::str_remove_all(fn, 'check_')
  progress_msg <- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                         ansi_align(glue("{m_counter}"), ansi_nchar(1)), " ",
                         ansi_align(glue("{passing}"), ansi_nchar(1)), " "
  )
  
  # run checks
  tryCatch(withCallingHandlers(
    {
      progress <- cli_progress_step("{progress_msg}", spinner = TRUE) # prints message
      rlang::exec(fn, .df) # runs check function
    },
    message = function(m) {
      # update counter if a galax message is triggered
      if (inherits(m, "galax_message")) {
        m_counter <<- m_counter + 1
        msgs <<- append(msgs, m$message)
        passing <<- col_red(symbol$cross)
        progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                               ansi_align(glue("{m_counter}"), ansi_nchar(1)), " ",
                               ansi_align(glue("{passing}"), ansi_nchar(1)), " "
        )
        cli::cli_progress_update(id = progress)
        rlang::cnd_muffle(m)
      } else {
        passing <<- col_green(symbol$tick)
        progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                                ansi_align(glue("{m_counter}"), ansi_nchar(1)), " ",
                                ansi_align(glue("{passing}"), ansi_nchar(1)), " "
        )
      }
    }),
    
    finally = {
      # capture all messages somewhere
      results <- tibble(
        term = fn_name,
        check_function = fn,
        messages = msgs
      )
      return(results)
    }
  )
}

#' Format each saved message from `check_all()` nicely
#' 
#' @noRd
#' @keywords Internal
format_messages_from_checks <- function(df) {
  # retrieve term & message
  term <- df$term |> unique()
  m <- paste0(df$messages)
  
  # format & print
  cat_line()
  cli::cli_rule("Error in {term}")
  cat_line()
  cat_line(m)
}

#' Build `check_all()` summary message
#' 
#' @noRd
#' @keywords Internal
summary_message <- function(results, checkable_fields) {
  n_errors <- length(results$messages)
  n_passing_fields <- length(checkable_fields) - length(unique(results$term))
  
  # message
  cat_line()
  cat_line(glue("Summary: {col_red(n_errors)} errors, {col_green(n_passing_fields)} passing"))
}


#' Advanced `check_all()` with separate message, warning, error tracking
#' @noRd
#' @keywords Internal
check_all_advanced <- function(fn, .df, checkable_fields) {
  
  # message saving & counting setup
  m_counter <- 0
  w_counter <- 0
  e_counter <- 0
  msgs <- list()
  wrns <- list()
  errs <- list()
  all_results <- list()
  
  # message format setup
  field_nchar <- max(ansi_nchar(checkable_fields))
  fn_name <- stringr::str_remove_all(fn, 'check_')
  progress_msg <- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                         ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
                         ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
                         ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
  )
  
  # run check functions
  tryCatch(withCallingHandlers(
    {
      progress <- cli::cli_progress_step("{progress_msg}", spinner = TRUE) # prints message
      rlang::exec(fn, .df) # runs check function
    },
    message = function(m) {
      if (inherits(m, "galax_message")) {
        m_counter <<- m_counter + 1
        msgs <<- append(msgs, m$message)
        progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                                ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
                                ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
                                ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
        )
        cli::cli_progress_update(id = progress)
        rlang::cnd_muffle(m)
      }
    },
    warning = function(w) {
      if (inherits(w, "galax_warning")) {
        w_counter <<- w_counter + 1
        wrns <<- append(wrns, w$message)
        progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                                ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
                                ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
                                ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
        )
        cli::cli_progress_update(id = progress)
        rlang::cnd_muffle(w)
      }
    }),
    error = function(e) {
      if (inherits(e, "galax_error")) {
        e_counter <<- e_counter + 1
        errs <<- append(errs, e)
        progress_msg <<- paste0(ansi_align(glue("{fn_name}"), field_nchar), " ",
                                ansi_align(glue("{m_counter}"), ansi_nchar(2)), " ",
                                ansi_align(glue("{w_counter}"), ansi_nchar(2)), " ",
                                ansi_align(glue("{e_counter}"), ansi_nchar(2)), " "
        )
        cli::cli_progress_update(id = progress)
        rlang::cnd_muffle(e)
      }
    },
    
    finally = {
      # capture all messages somewhere
      tibble(
        term = fn_name,
        check_function = fn,
        messages = msgs,
        warnings = wrns,
        errors = errs
      )
    }
  )
}


#' build suggestion message
#' @noRd
#' @keywords Internal
function_suggest_message <- function(suggested_functions,
                                     is_sf,
                                     .envir = parent.frame()) {
  
  # if POINT sf class, suggest `use_coordinates_sf()`
  if(!is.null(is_sf)) {
    if(isTRUE(is_sf)) {
      # add
      suggested_functions <- c("use_sf()", suggested_functions)
    }
  }
  
  # add pipe when there are multiple suggested functions
  if(length(suggested_functions) > 1) {
    suggested_functions_piped <- c(paste0(head(suggested_functions, -1), " |> "), tail(suggested_functions, 1))
  } else {
    suggested_functions_piped <- suggested_functions
  }
  
  # test whether user doesn't need any additional functions
  workflow_is_empty <- rlang::is_empty(suggested_functions_piped)
  
  if(!any(workflow_is_empty)) {
    cat_line(style_italic(paste0("\n", "Fix errors using the following workflow:", "\n")))
    cli_text("df |>")
    cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
    lapply(suggested_functions_piped, cli_alert, .envir = .envir)
    cli_end()
    
  } else {
    cat_line(paste0("\n", add_emoji(), " ", col_green("Your dataframe is Darwin Core compliant!"), "\n"))
    cat_line(paste0("Run checks, or use your dataframe to build a Darwin Core Archive:\n"))
    cli_text("df |>")
    cli_div(theme = list(.alert = list(`margin-left` = 2, before = "")))
    lapply(paste0("check_dataset()"), cli_alert, .envir = .envir)
    cli_end()
  }
}








