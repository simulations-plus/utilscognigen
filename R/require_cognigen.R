#' Require that the current R session is at Cognigen.
#' 
#' This is used to gracefully exit from functions that are not expected to work
#' outside of Cognigen.
#' 
#' Checks that "/misc" is available.
#'
#' @keywords internal
require_cognigen <- function() {
  
  if(!dir.exists("/misc")) {
    cli::cli_abort(
      message = "This function is only intended for use at Cognigen.",
      call = rlang::caller_env()
    )
  }
  
}
