#' Interface to Cognigen zfs snapshots
#'
#' @description
#' See
#' \href{https://wiki.cognigencorp.com/display/Office/DIY+File+Restoration}{Wiki
#' DIY File Restoration}
#'
#' \code{file_snapshots} gets the available paths to snapshots of a file on a
#' given snapshot date, month, or year.
#'
#' \code{diff_snapshot} produces a diff of a file and a single snapshot. If
#' RStudio is available, the diff is rendered in the Viewer. Otherwise, a Unix
#' \code{diff} is printed to the console.
#'
#' \code{restore_snapshot} restores a file using a single snapshot. By default,
#' this function restores a file to its most recent snapshot, saves the current
#' version in \code{tempdir()}, and outputs the result of \code{diff_snapshot}.
#' Directories cannot be restored.
#'
#' @param path a file name or path. Defaults to the path of the source editor
#'   context.
#'   
#' @param date a \code{character} in YYYY-MM-DD format or \code{Date} specifying
#'   the date range to subset to. Complete dates are not required;
#'   \code{character} representations of years or months are accepted. Defaults
#'   to the current date.
#'   
#' @param time \code{character} assumed to be in 24-hour format; will be
#'   adjusted if "AM" or "PM" is included. If a time is provided, the closest
#'   matching snapshot time is selected. "first" and "last" are also accepted.
#'   When more than a single \code{date} is provided, \code{time} should be
#'   either NULL, "first", or "last".
#' 
#' @param new_path an optional file name or path indicating where to save the
#'   restored snapshot. If provided, the original \code{path} will not be
#'   modified.
#'   
#' @param diff \code{logical} indicating whether to call
#'   \code{\link{diff_snapshot}}
#'   
#' @inheritParams file_copy
#'
#' @return
#' \code{file_snapshots} -- a \code{character} vector of file snapshots for a
#' given \code{date}. If \code{time} is provided, no more than one file snapshot
#' will be returned.
#'
#' \code{diff_snapshot} -- invisibly returns \code{NULL}
#'
#' \code{restore_snapshot} -- a \code{logical} indicating whether the copy
#' command was successful
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file_snapshots(path = "script.R", date = Sys.date())
#' file_snapshots(date = "2020-01-01")
#'
#' # check the differences between the latest snapshot and the current
#' # version of the current source document
#' diff_snapshot()
#'
#' # restore the current source document to the most recent snapshot
#' restore_snapshot()
#'
#' # restore a file to a snapshot on a given date, closest to a given time
#' restore_snapshot("file.R", date = "2020-02-19", time = "10:30")
#' 
#' # copy a restored file to a different directory or file
#' restore_snapshot("file.R", new_path = "archive/")
#' }
file_snapshots <- function(path = NULL, date = Sys.Date(), time = NULL) {
  
  require_cognigen()
  
  assertthat::assert_that(
    .Platform$OS.type == "unix",
    length(path) <= 1
  )
  
  path <- if(is.null(path)) get_source_file() else path
  path <- normalizePath(file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path)), mustWork = FALSE)
  
  split_path <- unlist(fs::path_split(path))
  
  # the full file path that follows /
  last_base_index <- ifelse("cognigen" == split_path[[3]], 3, 2)
  base_directory <- Reduce(file.path, split_path[1:last_base_index])
  base_directory <- gsub("/+", "/", base_directory)
  
  # the snapshot directory file path part that will come after the /misc,
  # /cognigen, or /home directory
  snapshot_glob <- ".zfs/snapshot/*"
  
  # the part of the file path that will come after the snapshot directories
  # (original path with the base_directory removed)
  after_snapshot_datetime <- Reduce(file.path, split_path[-c(1:last_base_index)])
  
  # glob of possible snapshot files
  path_glob <- file.path(base_directory, snapshot_glob, after_snapshot_datetime)
  
  # all absolute paths to snapshots (not subset to date or time)
  all_snapshot_paths <- Sys.glob(path_glob)
  
  if(length(all_snapshot_paths) == 0) {
    cli::cli_alert_info("No available snapshots for {.file {path}}")
    return(character())
  }
  
  # expected to extract the YYYY-MM-DD_HHMM that follows
  # ".zfs/snapshot/snap_daily-" or ".zfs/snapshot/snap_monthly-"
  all_snapshot_datetimes <- gsub(".*/snapshot/snap_[a-z]+-([^/]+).*", "\\1", all_snapshot_paths)
  all_snapshot_dates <- gsub("_.*", "", all_snapshot_datetimes)
  
  # try to parse date as entire date
  parsed <- FALSE
  clean_date <- lubridate::ymd(date, quiet = TRUE)
  
  # If full date was provided
  if(!is.na(clean_date) && !parsed) {
    first_snapshot_date <- last_snapshot_date <- clean_date
    parsed <- TRUE
  } else if(!parsed) {
    # try to parse as year-month if still missing
    datetime <- lubridate::parse_date_time(date, "ym", quiet = TRUE)
    y <- lubridate::year(datetime)
    m <- lubridate::month(datetime)
    clean_date <- ifelse(is.na(y) || is.na(m),
                         NA,
                         paste0(y, "-", ifelse(nchar(m) == 1, paste0("0", m), m)))
  }
  
  # if year-month was provided
  if(!is.na(clean_date) && !parsed) {
    datetime <- lubridate::parse_date_time(date, "ym", quiet = TRUE)
    y <- lubridate::year(datetime)
    m <- lubridate::month(datetime)
    
    first_snapshot_date <- lubridate::parse_date_time(clean_date, "ym", quiet = TRUE)
    last_snapshot_date <- lubridate::parse_date_time(paste0(y, "-", m + 1), "ym", quiet = TRUE) - 1
    
    parsed <- TRUE
    
  } else if(!parsed) {
    # try to parse as year if still missing
    datetime <- lubridate::parse_date_time(date, "y", quiet = TRUE)
    y <- lubridate::year(datetime)
    clean_date <- ifelse(is.na(y), NA, y)
  }
  
  # if year was provided
  if(!is.na(clean_date) && !parsed) {
    first_snapshot_date <- lubridate::parse_date_time(clean_date, "y", quiet = TRUE)
    last_snapshot_date <- lubridate::parse_date_time(y + 1, "y", quiet = TRUE) - 1
    
    parsed <- TRUE
    
  }
  
  if(is.na(clean_date)) {
    cli::cli_abort("Date {.val {date}} could not be parsed")
  }
  
  first_snapshot_date <- as.Date(first_snapshot_date)
  last_snapshot_date <- as.Date(last_snapshot_date)
  
  dates <- as.character(seq.Date(
    from = first_snapshot_date,
    to = last_snapshot_date,
    by = "days"
  ))
  
  # subset to matched dates
  which_dates_match <- which(all_snapshot_dates %in% dates)
  
  if(length(which_dates_match) == 0) {
    date_range_language <- ifelse(
      first_snapshot_date == last_snapshot_date,
      paste0("on ", first_snapshot_date),
      paste0("between ", first_snapshot_date, " and ", last_snapshot_date)
    )
    cli::cli_alert_info("No available snapshots {date_range_language} for {.file {path}}")
    return(character())
  }
  
  match_date_snapshot_paths <- all_snapshot_paths[which_dates_match]
  match_date_snapshot_datetimes <- all_snapshot_datetimes[which_dates_match]
  
  # sortable date/time object
  match_date_snapshot_datetimes_lubridate <- lubridate::ymd_hm(match_date_snapshot_datetimes)
  match_date_sort_order <- order(match_date_snapshot_datetimes_lubridate)
  
  # sort remaining vectors
  match_date_snapshot_datetimes_lubridate <- match_date_snapshot_datetimes_lubridate[match_date_sort_order]
  match_date_snapshot_paths <- match_date_snapshot_paths[match_date_sort_order]
  match_date_snapshot_datetimes <- match_date_snapshot_datetimes[match_date_sort_order]
  
  # extract times from sorted datetimes
  match_date_snapshot_times <- gsub(".*_", "", match_date_snapshot_datetimes)
  
  # subset to a single path if a time is provided
  result <- if(is.null(time)) {
    match_date_snapshot_paths
  } else if(!is.character(time) || length(time) != 1) {
    cli::cli_abort(
      "{.arg time} must be either NULL or a character with length 1."
    )
  } else if(time == "first") {
    match_date_snapshot_paths[[1]]
  } else if(time == "last") {
    match_date_snapshot_paths[[length(match_date_snapshot_paths)]]
  } else if(length(dates) > 1) {
    cli::cli_abort(
      "When more than one {.arg date} is included, {.arg time} should be either NULL, {.val first} or {.val last}."
    )
  } else {
    # adjust time so the later snapshot is selected on exact time-differences
    time_diffs <- abs(
      time_as_numeric(match_date_snapshot_times) - (time_as_numeric(time) + .0001)
    )
    match_date_snapshot_paths[[which.min(time_diffs)]]
  }
  
  result
  
}


#' @rdname file_snapshots
#' @export
diff_snapshot <- function(path = NULL, date = Sys.Date(), time = "last") {
  
  path <- if(is.null(path)) get_source_file() else path
  path <- normalizePath(file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path)), mustWork = FALSE)
  
  snapshot <- file_snapshots(
    path = path,
    date = date,
    time = time
  )
  
  if(length(snapshot) == 0) {
    # message already output from file_snapshots
    return(invisible(NULL))
  } else if(!file.exists(path) & length(snapshot) > 0) {
    cli::cli_alert_info("File no longer exists: {.file {path}}")
    return(invisible(NULL))
  } else if(!file.exists(path)) {
    cli::cli_alert_info("File does not exist: {.file {path}}")
    return(invisible(NULL))
  }
  
  if(length(snapshot) > 1) {
    cli::cli_warn("More than 1 snapshot was identified. Using the latest snapshot for diff.")
    snapshot <- snapshot[[length(snapshot)]]
  }
  
  if(dir.exists(path)) {
    system2("diff", c("-rq", path, snapshot))
    return(invisible(NULL))
  }
  
  if(rstudioapi::isAvailable()) {
    diffr_result <- suppressWarnings(
      diffr::diffr(snapshot, path, after = paste0(path, " (when called)"))
    )
    
    print_result <- try(
      print(diffr_result),
      silent = TRUE
    )
    
    if(inherits(print_result, "try-error")) {
      cli::cli_warn("Could not produce a diff of file: {.file {path}}")
      return(invisible(NULL))
    }
    
    return(invisible(NULL))
  } else {
    system2("diff", c("-r", path, snapshot))
    return(invisible(NULL))
  }
  
}


#' @rdname file_snapshots
#' @export
restore_snapshot <- function(path = NULL,
                             date = Sys.Date(),
                             time = "last",
                             args = c("-p", "-r"),
                             new_path = NULL,
                             diff = TRUE) {
  
  path <- if(is.null(path)) get_source_file() else path
  path <- normalizePath(file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path)), mustWork = FALSE)
  
  if(diff) {
    diff_snapshot(path, date, time)
  }
  
  snapshot <- file_snapshots(
    path = path,
    date = date,
    time = time
  )
  
  if(length(snapshot) == 0) {
    # message already output from file_snapshots
    return(invisible(NULL))
  } else if(length(snapshot) > 1) {
    cli::cli_warn("More than 1 snapshot was identified. Will restore the latest snapshot.")
    snapshot <- snapshot[[length(snapshot)]]
  }
  
  # copy files
  
  # path of new file
  if(!is.null(new_path)) {
    assertthat::assert_that(
      is.character(new_path),
      length(new_path) == 1,
      msg = "`new_path` should be either NULL or a single path to write the snapshot to."
    )
  }
  
  new_path <- ifelse(is.null(new_path), path, new_path)
  new_path <- ifelse(dir.exists(new_path), file.path(new_path, basename(path)), new_path)
  new_path <- normalizePath(file.path(normalizePath(dirname(new_path), mustWork = FALSE), basename(new_path)), mustWork = FALSE)
  
  # copy snapshot to new_path (could be original path if new_path was NULL)
  if(file.exists(new_path)) {
    temp_file <- file.path(tempdir(), basename(new_path))
    if(file.exists(temp_file)) {
      fs::file_delete(temp_file)
    }
    result_temp <- file_copy(new_path, temp_file, args = args)
    if(result_temp) {
      cli::cli_alert_success("Wrote {.file {new_path}} to {.file {temp_file}}")
    } else {
      cli::cli_abort("Failed to write {.file {new_path}} to {.file {temp_file}}")
    }
    fs::file_delete(new_path)
  }
  
  result_snapshot <- file_copy(snapshot, new_path, args = args)
  
  if(result_snapshot) {
    cli::cli_alert_success("Wrote {.file {snapshot}} to {.file {new_path}}")
  } else {
    cli::cli_abort("Failed to write {.file {snapshot}} to {.file {new_path}}")
  }
  
  return(result_snapshot)
  
}


# Helpers -----------------------------------------------------------------

#' Convert character representations of time to a numeric in the interval (0, 24].
#'
#' @param time \code{character} vector of times assumed to be in 24-hour format;
#'   will be adjusted if "AM" or "PM" is included.
#'
#' @return \code{numeric} vector of times in the interval (0, 24]
#'
#' @keywords internal
time_as_numeric <- function(time) {
  
  assertthat::assert_that(
    is.character(time)
  )
  
  if(any(grepl("am", time, ignore.case = TRUE) & grepl("pm", time, ignore.case = TRUE))) {
    cli::cli_abort("Both AM and PM cannot be provided to {.arg time}.")
  }
  
  datetime <- lubridate::parse_date_time(time, c("HM", "HMS", "H"), quiet = TRUE)
  
  # hour and minutes assuming 24-hour format
  h <- lubridate::hour(datetime)
  m <- lubridate::minute(datetime)
  
  # adjustment to hours if am or pm was provided (lubridate ignores am/pm).
  # for 12 AM, subtract 12 hours.
  # for all PM except for 12 PM, add 12 hours.
  adjust_am_pm <- ifelse(
    grepl("am", time, ignore.case = TRUE) & h == 12,
    -12,
    ifelse(
      grepl("pm", time, ignore.case = TRUE) & h != 12,
      12,
      0
    )
  )
  
  h <- h + adjust_am_pm
  
  time_numeric <- h + (m / 60)
  
  if(any(is.na(time_numeric))) {
    failed_to_parse <- time[which(is.na(time_numeric))]
    cli::cli_abort(
      c(
        "The following time(s) failed to parse: ",
        set_all_names(failed_to_parse, "x")
      )
    )
  }
  
  time_numeric
  
}
