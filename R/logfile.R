#' Open R, Rmd, SAS, tex, Rnw log files in RStudio for the current source
#' document
#'
#' @param ... file paths to open the log files of. Defaults to the path of the
#'   source editor context.
#'
#' @return invisibly returns \code{NULL}
#'
#' @export
logfile <- function(...) {

  assertthat::assert_that(
    rstudioapi::isAvailable()
  )

  paths <- unlist(list(...))
  paths <- if(is.null(paths)) get_source_file() else paths

  invisible(
    mapply(function(path, extension) {
      switch(
        extension,
        R = logfile_R(path),
        r = logfile_r(path),
        Rmd = logfile_Rmd(path),
        sas = logfile_sas(path),
        tex = logfile_tex(path),
        Rnw = logfile_Rnw(path)
      )
    },
    path = paths,
    extension = tools::file_ext(paths))
  )

  return(invisible(NULL))

}


# helpers -----------------------------------------------------------------

logfile_R <- function(path) {
  log_name <- paste0(tools::file_path_sans_ext(path), ".Rout")
  if(!file.exists(log_name)) {
    cli::cli_abort(
      c(
        "{.file {log_name}} has not been created.",
        i = "Generate a log file with {.fn rcb} from the R console or {.code rcb {basename(path)}} from the Terminal within the script directory."
      )
    )
  }
  rstudioapi::navigateToFile(log_name)
  return(invisible(NULL))
}

logfile_r <- function(path) {
  log_name <- paste0(path, ".Rout")
  if(!file.exists(log_name)) {
    cli::cli_abort(
      c(
        "{.file {log_name}} has not been created.",
        i = "Generate a log file with {.fn rcb} from the R console or {.code rcb {basename(path)}} from the Terminal within the script directory."
      )
    )
  }
  rstudioapi::navigateToFile(log_name)
  return(invisible(NULL))
}

logfile_Rmd <- function(path) {
  log_name <- paste0(tools::file_path_sans_ext(path), "-render.Rout")
  if(!file.exists(log_name)) {
    cli::cli_abort(
      c(
        "{.file {log_name}} has not been created.",
        i = "Generate a log file with {.fn render} from the R console."
      )
    )
  }
  rstudioapi::navigateToFile(log_name)
  return(invisible(NULL))
}

logfile_sas <- function(path) {
  log_name <- paste0(tools::file_path_sans_ext(path), c(".log", ".lst"))
  log_exists <- file.exists(log_name)
  if(!log_exists[1]) {
    cli::cli_abort(
      c(
        "{.file {log_name[1]}} has not been created.",
        i = "Generate a log file with {.code sas94 +we {basename(path)}} from the Terminal within the script directory."
      )
    )
  }
  log_name <- log_name[log_exists]
  invisible(lapply(log_name, rstudioapi::navigateToFile))
  return(invisible(NULL))
}

logfile_tex <- function(path) {
  log_name <- paste0(tools::file_path_sans_ext(path), ".log")
  if(!file.exists(log_name)) {
    cli::cli_abort(
      c(
        "{.file {log_name}} has not been created."
      )
    )
  }
  rstudioapi::navigateToFile(log_name)
  return(invisible(NULL))
}

logfile_Rnw <- logfile_tex
