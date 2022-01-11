#' Execute R programs in batch mode
#'
#' @description Calls \code{\link[callr]{rcmd}} on all files from each files'
#' parent directory. If the execution of any program is unsuccessful, later
#' programs are not executed. Command line equivalent for each run is \code{R
#' CMD BATCH --no-save --no-environ --no-init-file --no-restore script.R}
#'
#' In RStudio sessions, programs are executed as jobs so the R Console remains
#' available.
#'
#' @param ... file paths of R programs. Defaults to the path of the source
#'   editor context.
#' @param scanlogs \code{logical} indicating whether to execute
#'   \code{\link{scanlogs}} on Rout log files.
#' @param as_job \code{logical} indicating whether to execute as an RStudio job.
#'
#' @return When executed as an RStudio job, \code{rcb} invisibly returns
#'   \code{NULL} after submitting the job. Otherwise returns a \code{logical}
#'   vector indicating whether each program executed successfully.
#' @export
#'
#' @examples
#' \dontrun{
#' rcb("script1.R", "script2.R")
#' }
rcb <- function(..., scanlogs = TRUE, as_job = rstudioapi::isAvailable()) {

  assertthat::assert_that(
    is.logical(scanlogs)
  )

  files <- c(...)

  files <- if(is.null(files)) get_source_file() else files

  # Fail if any files do not exist
  files_exist <- file.exists(files)
  if(any(!files_exist)) {
    stop("File(s) do not exist: ", paste0(files[!files_exist], collapse = ", "))
  }

  # Fail if any files are not R files
  files_exts_r <- tools::file_ext(files) %in% c("r", "R")
  if(any(!files_exts_r)) {
    stop("File(s) are not R files: ", paste0(files[!files_exts_r], collapse = ", "))
  }

  files <- normalizePath(files)

  # Submit as a job for RStudio sessions
  if(isTRUE(as_job) && rstudioapi::isAvailable()) {
    rstudioapi::verifyAvailable(version_needed = "1.2")
    job_file <- fs::file_temp(pattern = "rcb-", ext = "R")
    rcb_call <- paste0("rcb(\n", paste0('  "', files, '"', collapse = ", \n"), "\n)")

    Redit(
      job_file,
      purpose = "temporary program to submit R program(s) as batch jobs",
      input_files = FALSE,
      output_files = FALSE,
      open = FALSE
    )

    cat(
      "\n",
      'library(utilscognigen)',
      "\n\n",
      rcb_call, "\n",
      sep = "",
      file = job_file,
      append = TRUE
    )

    rstudioapi::jobRunScript(job_file)

    message("Submitted job '", basename(job_file), "'")

    return(invisible(NULL))
  }

  executed_status <- logical(length(files))
  names(executed_status) <- files

  message("Executing R program(s) in batch mode ...")

  # Execute programs
  for(i in seq_along(files)) {
    f <- files[i]
    status <- .rcb(f)
    executed_status[i] <- status

    # If any executions are unsuccessful, stop execution, notify user, run scanlogs
    if(isFALSE(status)) {

      if(i < length(files)) {
        executed_status[(i + 1):length(files)] <- NA
        message("Execution of ", f, " failed. No additional programs will be run.")
      } else {
        message("Execution of ", f, " failed.")
      }

      if(scanlogs) scanlogs_if_not_empty(files[1:i])

      return(executed_status)
    }

    message("Executed '", f, "'")

  }

  Sys.sleep(3)

  if(scanlogs) scanlogs_if_not_empty(files[1:i])

  return(executed_status)

}


#' Execute an R script in batch mode
#'
#' @param path file path of R program
#'
#' @return \code{logical} indicating whether the program executed successfully
#' @keywords internal
.rcb <- function(path) {

  path <- normalizePath(path, mustWork = TRUE)

  callr_result <- try(
    callr::rcmd(
      cmd = "BATCH",
      cmdargs = c("--no-save", "--no-environ", "--no-init-file", "--no-restore", path),
      libpath = NULL,
      system_profile = TRUE,
      user_profile = FALSE,
      wd = dirname(path),
      fail_on_status = TRUE
      ),
    silent = TRUE
    )

  if(class(callr_result) == "try-error") {
    return(FALSE)
  } else if(callr_result$status != 0) {
    return(FALSE)
  }

  return(TRUE)

}



# Helpers -----------------------------------------------------------------

# Function to supress printed output
# From http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# Execute scanlogs, but only print if there are any results
# Also prints extra breaks
scanlogs_if_not_empty <- function(...) {
  sl <- quiet(scanlogs(...))
  sl <- Filter(function(x) !is.null(x), sl)
  if(length(sl)) {
    scanlogs_header <- paste0("\nscanlogs:\n", paste0(rep("=", getOption("width")), collapse = ""), sep = "\n")
    browser()

    # add extra breaks
    if(is.list(sl)) {
      sl <- unlist(lapply(sl, function(x) list(x, "\n")))
    }
    sl <- paste0(sl, collapse = "\n")
    sl <- gsub("\n\n\n+", "\n\n", sl)

    cat(
      scanlogs_header,
      sl,
      "\n",
      sep = ""
    )
  }
  return(invisible(NULL))
}
