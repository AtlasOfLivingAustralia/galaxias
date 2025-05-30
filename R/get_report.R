#' @rdname check_archive
#' @param obj Either an object of class `character` containing a key that 
#' uniquely identifies your query; or an object of class `gbif_validator`.
#' returned by [check_archive()] or [get_report()]
#' @param n Maximum number of entries to print per file. Defaults to 5.
#' @order 2
#' @export
get_report <- function(obj,
                       n = 5,
                       wait = TRUE,
                       quiet = FALSE){
  
  # check class of supplied object
  if(inherits(obj, "gbif_validator")){
    key <- obj$key
  }else if(inherits(obj, "character")){
    key <- obj
  }else{
    cli::cli_abort("Argument {.arg obj} must be of class {.cls character} or {.cls gbif_validator}")
  }
  
  # run query
  # NOTE: This approach means that this function *always* hits the API
  # If you don't want that, call `view_report()` instead.
  gbif_response <- query_gbif_validator_api(key)

  # quick error catcher
  if(is.null(gbif_response)){
    cli::cli_abort("Something went wrong with your query")
  }
  
  # set behaviour depending on `status`
  if(is_gbif_validator_complete(gbif_response)){
    if(!quiet){
      print_archive_status(gbif_response)
      print_archive_issues(gbif_response, n = n)      
    }
    gbif_response
  }else{
    if(wait){
      delayed_response <- wait_for_gbif_response(gbif_response,
                                                 quiet = quiet) 
      if(!quiet){
        print_archive_status(delayed_response)
        print_archive_issues(delayed_response, n = n)
      }
      delayed_response
    }else{
      if(!quiet){
        print_archive_status(gbif_response)
      }
      gbif_response
    }
  }
}


#' Internal function to run the actual query
#' @noRd
#' @keywords Internal
query_gbif_validator_api <- function(key){
  result <- glue::glue("https://api.gbif.org/v1/validation/{key}") |>
    httr2::request() |>
    httr2::req_options(
      httpauth = 1,
      userpwd = gbif_username_string()) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  class(result) <- c("gbif_validator", "list")
  result
}


#' Internal function to take GBIF 'status' and decide whether result is complete
#' @noRd
#' @keywords Internal
is_gbif_validator_complete <- function(x){
  status <- tolower(x$status)
  if(status %in% c("downloading", "submitted", "running", "queued")){
    FALSE
  }else if(status %in% c("finished", "aborted", "failed")){
    TRUE
  }else{
    TRUE
    # Note: TRUE catches 'unknown' values in this scenario;
    # this may be unnecessary, but is desirable to prevent infinite loops
  }
}


#' Internal function to wait for a response from GBIF
#' @param x object of class "gbif_validator"
#' @noRd
#' @keywords Internal
wait_for_gbif_response <- function(x,
                                   quiet = FALSE){
  # set up queue management
  rate_object <- purrr::rate_backoff(pause_base = 0.5, 
                                     pause_cap = 60, 
                                     max_times = 100,
                                     jitter = FALSE)
  continue <- TRUE
  current_status <- x$status
  iter <- 1
  if(!quiet){
    cli_progress_bar()
  }

  # queuing is actually a `while` loop with multiple conditions
  while(continue == TRUE){
    x <- query_gbif_validator_api(x)
    if(!quiet){
      cli_progress_update()  
    }
    continue <- !is_gbif_validator_complete(x)
    if(continue){
      iter <- iter + 1
      if(iter < 100){
        rate_sleep(rate_object, quiet = quiet)
      }else{
        cli::cli_inform(
          c("No data were returned after 100 tries.", 
            i = "You can check again by calling `check_report(key = \"{x$key}\")")) 
        x        
      }
    }else{
      if(!quiet){
        cli_progress_done()
      }
      x
    }
  }
}