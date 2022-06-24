#' Copy files and directories using Unix \code{cp} command
#'
#' This function is designed to use the Unix \code{cp} command, but will revert
#' to \code{base::file.copy()} in Windows environments. In this case, including
#' \code{"-r"} in \code{args} results in \code{recursive = TRUE}.
#'
#' @param from \code{character} vector containing file names or paths. Can also
#'   contain directories, but \code{args} must include \code{"-r"} (recursive).
#' @param to \code{character} vector containing file names, paths, or existing
#'   directories.
#' @param args \code{character} vector of additional arguments to \code{cp}. The
#'   default includes \code{"-p"}, to preserve mode, ownership, and timestamps;
#'   and \code{"-r"} to copy directories recursively. See \code{file_copy(args =
#'   "help")} for \code{cp} arguments.
#'
#' @return returns a \code{logical} vector indicating which copy operations
#'   succeeded for each of the files attempted.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get cp help to review optional args
#' file_copy(args = "help")
#'
#' # Copy a single file
#' file_copy("old-dir/file.R", "new-dir/file.R")
#'
#' # Copy multiple files to an existing directory
#' file_copy(c("old-dir/file1.R", "old-dir/file2.R"), "new-dir/")
#'
#' # Pass no optional arguments
#' file_copy("old-dir/file.R", "new-dir/file.R", args = NULL)
#'
#' # Pass optional arguments
#' file_copy("old-dir/file.R", "new-dir/file.R", args = c("-b", "-n", "-p"))
#' }
file_copy <- function(from, to, args = c("-p", "-r")) {

  if(.Platform$OS.type == "windows") {
    return(file.copy(from, to, recursive = ifelse("-r" %in% args, TRUE, FALSE)))
  }

  if(args[1] %in% c("--help", "-help", "help")) return(system2("cp", "--help"))

  assertthat::assert_that(
    .Platform$OS.type == "unix",
    length(from) > 0,
    length(to) > 0,
    all(file.exists(from))
    )


  if(length(from) == 1) {

    # Copy a single file to a single file or directory
    if(length(to) > 1) cli::cli_abort("More {.arg from} elements than {.arg to}")
    return(cp(from, to, args))

  } else if(length(from) > 1 && length(to) == 1) {

    # Copy multiple files to a single directory
    if(!dir.exists(to)) cli::cli_abort("Multiple {.arg from} provided with only one {.arg to}, but {.arg to} is not an existing directory.")
    return(vapply(
      X = from,
      FUN = cp,
      FUN.VALUE = logical(1),
      to = to,
      args = args,
      USE.NAMES = FALSE
    ))

  } else if(length(from) > 1 && length(to) > 1) {

    # Copy multiple files to multiple files
    if(length(from) != length(to)) cli::cli_abort("Multiple {.arg from} and {.arg to} provided, but they are not of equal length.")
    return(mapply(
      FUN = cp,
      from = from,
      to = to,
      args = args,
      USE.NAMES = FALSE
    ))

  }

}


# helpers -----------------------------------------------------------------

#' Execute \code{cp} command
#'
#' @param from a file name, path, or directory.
#' @param to a file name, path, or directory.
#' @param args \code{character} vector of additional arguments to \code{cp}. The
#'   default includes \code{"-p"}, to preserve mode, ownership, and timestamps.
#'   See \code{file_copy(args = "help")} for \code{cp} arguments.
#'
#' @return \code{logical} indicating if copy operation succeeded.
#'
#' @keywords internal
cp <- function(from, to, args = NULL) {
  if(!file.exists(from)) {
    cli::cli_abort("File does not exist: {.file {from}}")
  }

  res <- system2("cp", args = c(from, to, args))
  return(ifelse(res == 0, TRUE, FALSE))
}
