#' Require that the current R session is at CPP.
#' 
#' This is used to gracefully exit from functions that are not expected to work
#' outside of CPP.
#' 
#' @details
#' Checks that "/misc" is available.
#'
#' @keywords internal
require_cognigen <- function() {
  
  if(!is_cognigen()) {
    cli::cli_abort(
      message = "This function is only intended for use at CPP.",
      call = rlang::caller_env()
    )
  }
  
}


#' Check if the current R session is at CPP.
#'
#' @return logical
#' 
#' @keywords internal
is_cognigen <- function() {
  length(list.files("/cognigen")) > 0
}

#' Check if the current R session is at Simulations Plus division.
#'
#' @return logical
#' 
#' @keywords internal
is_slp_slp <- function() {
  length(list.files("/miguel")) > 0
}

#' Check if the current R session is at any Simulations Plus division.
#'
#' @return logical
#' 
#' @keywords internal
is_slp_any <- function() {
  is_cognigen() || is_slp_slp()
}
