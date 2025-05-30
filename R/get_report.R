#' @rdname check_archive
#' @param key A key that uniquely identifies your query
#' @order 2
#' @export
get_report <- function(key, 
                       wait = TRUE,
                       quiet = FALSE){

  gbif_response <- query_gbif_validator_api(key)
  if(is.null(gbif_response)){
    cli::cli_abort("Something went wrong with your query")
  }
  
  if(is_gbif_validator_complete()){
    if(quiet){
      invisible(gbif_response)
    }else{
      gbif_response
    }
  }else{
    if(wait){
      wait_for_gbif_response(gbif_response,
                             quiet = quiet)  
    }else{
      if(quiet){
        invisible(gbif_response)
      }else{
        gbif_response
      }
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
  class(result) <- c("gbif_response", "list")
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
#' @param x object of class "gbif_response"
#' @noRd
#' @keywords Internal
wait_for_gbif_response <- function(x,
                                   quiet = FALSE){
  # set up queue management
  rate_object <- purrr::rate_backoff(pause_base = 0.5, 
                                     pause_cap = 60, 
                                     max_times = 100,
                                     jitter = FALSE)
  continue <- !is_gbif_validator_complete()
  iter <- 1
  
  if(!quiet){
    cli::cli_text("Waiting for validator to complete.")
  }
  
  # queuing is actually a `while` loop with multiple conditions
  while(continue == TRUE){
    x <- query_gbif_validator_api(x)
    continue <- !is_gbif_validator_complete()
    if(continue){    
      iter <- iter + 1
      if(iter > 99){
        cli::cli_inform(
          c("No data were returned after 100 tries.", 
            i = "You can check again by calling `check_report(key = \"{x$key}\")")) 
        if(quiet){
          invisible(x)
        }else{
          x
        }
      }else{
        rate_sleep(rate_object, quiet = quiet)
      }
    }else{
      if(quiet){
        invisible(x)
      }else{
        x
      }
    }
  }
}