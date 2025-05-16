#' Submit a Darwin Core Archive to the ALA
#' 
#' The preferred method for submitting a dataset for publication via the ALA
#' is to raise an issue on our ['Data Publication' GitHub Repository](https://github.com/AtlasOfLivingAustralia/data-publication),
#' and attached your archive zip file (constructed using [build_archive()]) to 
#' that issue. If your dataset is especially large (>100MB), you will need to 
#' post it in a publicly accessible location (such as a GitHub release) and post 
#' the link instead. This function simply opens a new issue in the users' 
#' default browser to enable dataset submission.
#' @details
#' The process for accepting data for publication at ALA is not automated;
#' this function will initiate an evaluation process, and will not result in 
#' your data being instantly visible on the ALA. Nor does submission guarantee 
#' acceptance, as ALA reserves the right to refuse to publish data that reveals 
#' the locations of threatened or at-risk species.
#' @returns Does not return anything to the workspace; called for the side-effect
#' of opening a submission form in the users' default browser.
#' @export
submit_archive <- function(){
  if(rlang::is_interactive()){ 
    
    choice <- cli_menu(
      c(" ",
        "Running this function will open a data submission page in your default browser.", 
        "There you will be prompted to upload a Darwin Core Archive.", 
        " "),
      "Do you want to proceed? (0 to exit)",
      choices = c("Yes", "No")
    )
    
    if (choice == 1) {
      utils::browseURL("https://github.com/AtlasOfLivingAustralia/data-publication/issues/new?template=new-dataset.md")
    } else {
      cli::cli_inform(c(
        i = "Exiting..."
      ))
      # exits process quietly
      invokeRestart("abort")
    }
  } 
}