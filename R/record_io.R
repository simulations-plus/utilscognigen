#' Record and report input and output files used and created in R scripts
#'
#' @md
#'
#' @name recorded_io
#'
#' @param call a function call to read a data file or create a new file;
#'   alternatively, \code{call} can be one or more file paths.
#' @param quiet a logical value indicating whether messages should be printed
#'   (\code{FALSE}; default) or not (\code{TRUE}).
#' @param path a file path to an R, Rmd, qmd, or log file. Defaults to the path of
#'   the source editor context.
#' @param invisible a logical value indicating whether the function should
#'   return a value (\code{FALSE}; default) or not (\code{TRUE}). This is mostly
#'   useful with \code{source} calls
#'
#' @export
#'
#' @details ## Recording input and output files
#'
#' Calling either \code{record_input()} or \code{record_output()} will cause
#' information about file paths to be stored in a dedicated environment. If
#' \code{call} is a function call, both functions expect attempts to detect file
#' names and paths based upon known arguments of common functions that read or
#' creates files; if the argument of the \code{call} are not recognized, both
#' \code{record_input()} and \code{record_output()} will assume that the 1st
#' argument \code{call} provided the path of the file to be recorded.
#'
#' By default, a message will be printed by both \code{record_input()} and
#' \code{record_output()} to report the path of the files that were stored.
#'
#' ## Reporting input and output files
#'
#' Ultimately, the collected information can be summarized by calling
#' \code{recorded_io()} typically at the end of an R script. In an .Rmd file,
#' \code{recorded_io()} would need to be called inside a chunk set with the
#' \code{message = FALSE} option so the information could be printed to console
#' or the .Rout file when .Rmd file is rendered using
#' \code{\link[utilscognigen]{render}}.
#'
#' In some cases (eg, in an interactive R session), it could be useful to call
#' \code{clear_recorded_io()} to erase all the recorded input and output file
#' paths.
#'
#' Retrieve collected input and output files from the log of an executed R or
#' Rmd file with \code{get_recorded_io()}.
#'
#' @return 
#' \code{record_input()} returns the output of the evaluated \code{call}.
#'
#' \code{record_output()} invisibly returns \code{NULL} after evaluating
#' \code{call} which is expected to create one or more files as a side-effect of
#' the call.
#' 
#' \code{recorded_io()} and \code{clear_recorded_io()} both invisibly return
#' \code{NULL}.
#' 
#' \code{get_recorded_io()} returns a list with two character vector elements:
#' input_files and output_files.
#'
#' @examples
#'
#' \dontrun{
#' df <- record_input(
#'   read.csv(
#'     file = '/path/to/existing/file.csv',
#'     header = TRUE
#'   )
#' )
#' df <- record_input(
#'   haven::read_sas('/path/to/existing/file.sas7bdat')
#' )
#' record_input('../../some/adhoc/file.RDS')
#'
#' # Information will not be recorded if the call fails
#' df <- record_input(
#'   read.csv(
#'     file = '/path/to/invalid/file.csv',
#'     header = TRUE
#'   )
#' )
#'
#' record_output(
#'   png('Rplots_%03d.png')
#' )
#' plot(1) ; plot(2)
#' dev.off()
#'
#' recorded_io()
#' }
#' 
NULL

#' @rdname recorded_io
#' @export
record_input <- function(call, quiet = FALSE, invisible = FALSE){
  
  # Evaluate call first so that only valid calls are captured in record_files
  res <- suppressWarnings( call )
  
  # Capture call for input file reading
  scall <- substitute(call)
  
  # Process call
  if ( inherits(scall, 'call') && scall[[1]] != 'c' ){
    # If call is a function call, expand unnamed passed arguments
    scall <- match.call(eval(scall[[1]]), scall)
    record_io(call = scall, type ='input', quiet = quiet)
  } else {
    # call is coerced to a string
    sapply(
      call, 
      function(x) {
        record_io(file = as.character(x), type = 'input', quiet = quiet)
      }
    )
  }
  
  if ( invisible ){
    invisible()
  } else {
    return(res)
  }
  
}

#' @rdname recorded_io
#' @export
record_output <- function(call, quiet = FALSE){
  
  # Evaluate call first so that only valid calls are captured in record_files
  call
  
  # Capture call for output creation
  scall <- substitute(call)
  
  if ( inherits(scall, 'call') & !is.vector(call) ){
    # If call is a function call, expand unnamed passed arguments
    scall <- match.call(eval(scall[[1]]), scall)
    record_io(call = scall, type = 'output', quiet = quiet)
  } else {
    # call is coerced to a string
    sapply(
      call, 
      function(x) {
        record_io(file = as.character(x), type = 'output', quiet = quiet)
      }
    )
  }
  
  invisible()
  
}

#' Utility function supporting \code{record_input} and \code{record_output}
#' 
#' @inheritParams recorded_io
#' @param file a file path.
#' @param type either 'input' or 'output' to document whether the recorded
#' information corresponds to an input or output file.
#' 
#' @keywords internal
record_io <- function(call, file, type = 'input', quiet = FALSE){
  
  if ( !missing(file) ){
    
    file1 <- file
    file2 <- NULL
    nofile <- FALSE
    call <- file
    
  } else {
    
    # Get call arguments names
    arg_names <- names(as.list(call))[-1]
    
    # Reference argument list for primary file argument
    ref1 <- c(
      'file', 'filename', 'con', 'key', 'data_file', 'description', 'socket', 'url', 'xlsxFile', 'target', 'wb'
    )
    
    # Reference argument list for secondary file argument in haven::read_sas, mrgsolve::mread, or
    # mrgsolve::read_nmext
    ref2 <- c('catalog_file' , 'project')
    
    # Does the call include a path-like argument ?
    if ( 'path' %in% arg_names){
      path <- eval(call[[which(arg_names == 'path') + 1]])
    } else {
      path <- NULL
    }
    
    # Does the call include file-like argument from ref1 ?
    # Code assumes that the elements of ref1 are mutuality exclusive function arguments
    if ( any(arg_names %in% ref1) ){
      file1 <- do.call(
        file.path, 
        as.list(
          c(
            path,
            eval(call[[ which.min( match(arg_names, ref1) ) + 1]])
          )
        )
      )
    } else {
      file1 <- NULL
    }
    
    # Does the call include file-like argument from ref2 ?
    # Code assumes that the elements of ref2 are mutuality exclusive function arguments
    # and path is not combined with these arguments
    if ( any(arg_names %in% ref2) ){
      file2 <- eval(call[[ which.min( match(arg_names, ref2) ) + 1]])
    } else {
      file2 <- NULL
    }
    
    # If no argument is detected, assume that 1st argument of call is a file path
    nofile <- is.null(file1) & is.null(file2)
    if ( nofile ){
      if ( length(path) == 0 ){
        file1 <- eval(call[[2]])
      } else {
        file1 <- path
        nofile <- FALSE
      }
    }
    
  }
  
  # Check existence of environment - create one if it does not
  if ( !exists('ioenv', mode = 'environment') ){
    assign('ioenv', new.env(), envir = globalenv())
    assign(
      x = 'record_files',
      value = data.frame(
        io = character(),
        path = character(),
        call = character()
      ),
      envir = ioenv
    )
  }
  
  # Store input information
  io <- try(
    {
      rbind(
        get(x = 'record_files', envir = ioenv),
        data.frame(
          io = type,
          path = file1,
          call = paste0(trimws(deparse(call)), collapse = " ")
        )
      )
    },
    silent = TRUE
  )
  
  if ( inherits(io, 'try-error') ){
    cli::cli_alert_warning(
      'Invalid {type} information: {.file {file1}}'
    )
  } else {
    assign(x = 'record_files', value = io, envir = ioenv)
    if ( nofile ){
      cli::cli_alert_warning(
        'First argument of inner call recorded: {.file {file1}}'
      )
    } else {
      if ( !quiet ){
        file1 <- suppressWarnings(normalizePath(file1))
        snake_type <- snakecase::to_title_case(type)
        cli::cli_alert_info(
          '{snake_type} file recorded: {.file {file1}}'
        )
      }
    }
  }
  
  if ( !is.null(file2) ){
    io <- try(
      {
        rbind(
          get(x = 'record_files', envir = ioenv),
          data.frame(
            io = type,
            path = file2,
            call = deparse(call)
          )
        )
      },
      silent = TRUE
    )
    
    if ( inherits(io, 'try-error') ){
      cli::cli_alert_warning(
        'Invalid output information: {.file {file2}}'
      )
    } else {
      assign(x = 'record_files', value = io, envir = ioenv)
      if ( !quiet ){
        file2 <- suppressWarnings(normalizePath(file2))
        cli::cli_alert_info(
          'Output file(s) recorded: {.file {file2}}'
        )
      }
    }
  }
  
}

#' @rdname recorded_io
#' @export
recorded_io <- function(){
  
  if ( !exists('ioenv', mode = 'environment') ){
    cli::cli_alert_warning('No recorded input or output files')
    return(invisible())
  }
  
  record_files <- get(x = 'record_files', envir = ioenv)
  
  if ( nrow(record_files) == 0){
    cli::cli_alert_warning('No recorded input or output files')
    return(invisible())
  }
  
  # when called from R Markdown non-interactively, require that chunk option
  # message=FALSE. this results in the message being written to the log file.
  if( !interactive() ){
    source_file <- get_source_file()
    if( !is.null(source_file) ){
      if( tolower(tools::file_ext(source_file)) %in% c("rmd", "qmd") ) {
        if( !isFALSE(knitr::opts_current$get("message")) ) {
          cli::cli_abort("{.fn recorded_io} requires the chunk option {.code message=FALSE} when called from R Markdown or Quarto.")
        }
      }
    }
  }
  
  ios <- apply(
    X = record_files,
    MARGIN = 1,
    FUN = function(x){
      
      # Normalize file path
      file <- suppressWarnings(
        normalizePath(x[2])
      )
      
      # Convert from C-format to Regex
      # Current assumption in that C-format would come from device calls (eg png)
      file <- sub('%[0-9]+[dis]', '[\\\\s]*[0-9]+', file)
      
      # Expand files regex 
      files <- file.path(
        dirname(file),
        list.files(path = dirname(file), pattern = paste0('^', basename(file), '$') )
      )
      
      # Try to find files
      if ( length(files) > 0 && all(file.exists(files)) ){
        paste(
          sprintf('**[%s] %s', x[1], files),
          collapse = '\n'
        )
      } else {
        cli::cli_alert_warning(
          'Invalid file: {x[3]}'
        )
        NA
      }
    }
  )
  
  # Print to standard out unique values of ios
  ios <- unique(ios)
  ios <- ios[!is.na(ios)]
  
  if ( length(ios) > 0 ){
    cli::cli_alert_info(
      'List of recorded input and output files:'
    )
    message( paste(ios, collapse = '\n') )
  }
  invisible()
  
}

#' @rdname recorded_io
#' @export
clear_recorded_io <- function(){
  
  if ( exists('ioenv', mode = 'environment') ){
    assign(
      x = 'record_files',
      value = data.frame(
        io = character(),
        path = character(),
        call = character()
      ),
      envir = ioenv
    )
  }
  
  invisible()
  
}


#' @rdname recorded_io
#' @export
get_recorded_io <- function(path = NULL) {
  
  path <- if(is.null(path)) get_source_file() else path
  
  if(is.null(path)) {
    cli::cli_abort("An R, Rmd, qmd, or log file must be open or a path must be specified to use {.fn get_recorded_io}.")
  } else if(length(path) > 1) {
    cli::cli_abort("Only one file can be specified to use {.fn get_recorded_io}")
  } else if(!file.exists(path)) {
    cli::cli_abort("{.file {path}} does not exist.")
  }
  
  extension <- tools::file_ext(path)
  
  log_name <- switch(
    extension,
    Rout = path,
    R = paste0(tools::file_path_sans_ext(path), ".Rout"),
    r = paste0(path, ".Rout"),
    Rmd = paste0(tools::file_path_sans_ext(path), "-render.Rout"),
    qmd = paste0(tools::file_path_sans_ext(path), "-render.Rout")
  )
  
  if(!file.exists(log_name)) {
    
    if(endsWith(log_name, "-render\\.Rout")) {
      cli::cli_abort(
        c(
          "{.file {log_name}} has not been created.",
          i = "Generate a log file with {.fn render} from the R console."
        )
      )
    } else {
      code <- paste0("rcb", basename(path))
      cli::cli_abort(
        c(
          "{.file {log_name}} has not been created.",
          i = "Generate a log file with {.fn rcb} from the R console or {.code {code}} from the Terminal within the script directory."
        )
      )
    }
    
  }
  
  rout <- readLines(log_name, warn = FALSE)
  
  rout_recorded_input <- unique(grep(
    "^\\*{2}\\[input\\]", 
    x = rout, 
    value = TRUE
  ))
  
  rout_recorded_output <- unique(grep(
    "^\\*{2}\\[output\\]",
    x = rout,
    value = TRUE
  ))
  
  if(length(rout_recorded_input) == 0) {
    rout_recorded_input <- character()
  }
  
  if(length(rout_recorded_output) == 0) {
    rout_recorded_output <- character()
  }
  
  rout_recorded_input <- trimws(gsub("^\\*{2}\\[input\\]", "", rout_recorded_input))
  rout_recorded_output <- trimws(gsub("^\\*{2}\\[output\\]", "", rout_recorded_output))
  
  list(
    input_files = rout_recorded_input,
    output_files = rout_recorded_output
  )
  
}
