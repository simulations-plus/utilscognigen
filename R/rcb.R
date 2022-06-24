#' Execute R programs in batch mode
#'
#' @description Calls \code{\link[callr]{rcmd}} on all files from each files'
#' parent directory. If the execution of any program is unsuccessful, later
#' programs are not executed. Command line equivalent for each run is \code{R
#' CMD BATCH --no-save --no-environ --no-init-file --no-restore script.R}
#'
#' In RStudio sessions, programs can be executed as jobs so the R Console
#' remains available.
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
rcb <- function(..., scanlogs = TRUE, as_job = getOption("utilscognigen.rcb_as_job", TRUE)) {

  assertthat::assert_that(
    is.logical(scanlogs)
  )

  files <- c(...)

  if (is.null(files)){
    # rcb is called from interactive session
    files <- get_source_file()
    # save current document from interactive session
    rstudioapi::documentSave()
  }

  # Fail if any files do not exist
  files_exist <- file.exists(files)
  if(any(!files_exist)) {
    cli::cli_abort(
      c(
        "File(s) do not exist: ",
        set_all_names(files[!files_exist], "x")
      )
    )
  }

  # Fail if any files are not R files
  files_exts_r <- tools::file_ext(files) %in% c("r", "R")
  if(any(!files_exts_r)) {
    cli::cli_abort(
      c(
        "File(s) are not R files: ",
        set_all_names(files[!files_exts_r], "x")
      )
    )
  }

  files <- normalizePath(files)

  # Submit as a job for RStudio sessions
  if(isTRUE(as_job) && rstudioapi::isAvailable()) {
    rstudioapi::verifyAvailable(version_needed = "1.2")
    
    # name the job file after the R script being executed if only one script is
    # being executed
    if(length(files) == 1) {
      job_file <- file.path(tempdir(), paste0("rcb-", tools::file_path_sans_ext(basename(files)), ".R"))
      if(file.exists(job_file)) {
        file.remove(job_file)
      }
    } else {
      job_file <- fs::file_temp(pattern = "rcb-", ext = "R")
    }
    
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

    cli::cli_alert_info("Submitted job {.strong {basename(job_file)}}")

    return(invisible(NULL))
  }

  executed_status <- logical(length(files))
  names(executed_status) <- files

  cli::cli_alert_info("Executing R program(s) in batch mode ...")

  # Execute programs
  for(i in seq_along(files)) {
    f <- files[i]
    status <- .rcb(f)
    executed_status[i] <- status

    # If any executions are unsuccessful, stop execution
    if(isFALSE(status)) {

      if(i < length(files)) {
        executed_status[(i + 1):length(files)] <- NA
        cli::cli_bullets(
          c(
            x = "Execution failed for {.file {f}}",
            `!` = "No additional programs will be executed"
          )
        )
      } else {
        cli::cli_alert_danger("Execution failed for {.file {f}}")
      }
      
      # exit for loop
      break
      
    }

    cli::cli_alert_success("Executed {.file {f}}")

  }
  
  cli_executed_status <- set_cli_executed_status(executed_status)
  
  cli::cli_h1("rcb summary")
  cli::cli_bullets(cli_executed_status)
  cli::cli_verbatim("")
  cli_executed_status_key()
  
  if(scanlogs) print(scanlogs(files[1:i]))
  
  return(invisible(executed_status))

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

# Set the names for cli executed status
# x is expected to be a named logical vector that may contain NAs
# the names of x are filenames
set_cli_executed_status <- function(x) {
  lgl <- unname(x)
  files <- names(x)
  
  names(files) <- ifelse(
    is.na(lgl), "!",
    ifelse(
      lgl, "v", 
      ifelse(
        !lgl, "x", ""
      )
    )
  )
  
  files
}

cli_executed_status_key <- function() {
  
  cli::cli_verbatim("Execution Key:")
  
  cli::cli_text(
    c(
      "{cli::col_green(cli::symbol$tick)} Executed Successfully",
      " | {cli::col_red(cli::symbol$cross)} Execution Failed",
      " | {cli::col_yellow('!')} Execution Skipped"
    )
  )
}