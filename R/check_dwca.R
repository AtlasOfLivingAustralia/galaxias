#' pointblank-based checking of a `dwca` object
#' 
#' Description to follow.
#' @param .dwca An object of class `dwca`, created using `dwca()`
#' @param max_n Optional limit to the number of failed tests allowed
#' @importFrom pointblank col_exists
#' @importFrom pointblank create_agent
#' @importFrom pointblank get_agent_report
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export
check_dwca <- function(.dwca,
                  max_n = NULL){
  # error catching
  if(!inherits(.dwca, "dwca")){
    abort("`check` only accepts `dwca` objects")
  }
  
  # set up storage object
  report <- vector(mode = "list", length = length(.dwca))
  names(report) <- names(.dwca)
  
  # check occurrences data via {pointblank}
  if(.dwca$core$type == "Occurrences"){ # not sure of terminology or structure here yet
    check_occurrences(.dwca$data) 
  }
  # NOTE: at present, this only returns a tibble
  # Need to parse this to return either:
    # report to the console (as per `{testthat}`)
    # html report to the viewer (as per `{pointblank}`)
  # Q: should the above be separate functions?
  
  # metadata checking via `xml_validate()`
  if(is.null(.dwca$metadata)){
    inform("No metadata supplied") # or add this to report?
  }else{
    report$metadata <- check_metadata(.dwca$metadata)
  }
  
  report
}

# Build report in pieces - one for each check function
# Make functions return logical, and if true do nothing, false add to report
# Section 0 - Summary (percentage match, settings used, missing vals, seed)
# Section 1 - compliance
# Section 2 - interactive fixes performed
# Section 3 - further recommendations
# Save to working directory as plain markdown