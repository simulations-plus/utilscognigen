# Copyright 2022-$date Cognigen Corporation, a Simulations Plus Company

#' Utility function to collect the paths of input and output files used and 
#' created in a R script, and print a short report in the console or log file.
#' 
#' @name recorded_io
#' 
#' @param call a function call to read a data file or create a new file; 
#' alternatively, call could be a simple file path.
#' @param quiet a logical value indicating whether messages should be printed 
#' (FALSE; default) or not (TRUE).
#' 
#' @export
#' 
#' @return If \code{call} is a function call, \code{record_input} will execute
#' this call and returns its output. If \code{call} is a file path, 
#' \code{record_input} will simply return the value of \code{call}.
#' 
#' In \code{record_output}, \code{call} is expected to create one or more files
#' as a side-effect of the call. No return value is expected nor is returned.
#' 
#' Calling either \code{record_input} or \code{record_output} will cause 
#' information about file paths to be stored in a dedicated environment. If 
#' \code{call} is a function call, both functions expect attempts to detect 
#' file names and paths based upon known arguments of common functions that read
#' or creates files; if the argument of the \code{call} are not recognized, both
#' \code{record_input} and \code{record_output} will assume that the 1st argument 
#' \code{call} provided the path of the file to be recorded.
#' 
#' By default, a message will be printed by both \code{record_input} and 
#' \code{record_output} to report the path of the files that were stored.
#' 
#' Ultimately, the collected information can be summarized by calling 
#' \code{recorded_io} (typically at the end of R script). 
#' 
#' In some cases (eg, in an interactive R session), it could be useful to call
#' the \code{clear_recorded_io} function to erase all the recorded input and
#' output file paths.
#' 
#' @examples
#' 
#' \donttest{
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
record_input <- function(call, quiet = FALSE){

  # Evaluate call first so that only valid calls are captured in record_files
  res <- suppressWarnings( call )
  
  # Capture call for input file reading
  scall <- substitute(call)
  
  # Process call
  if ( class(scall) == 'call' && scall[[1]] != 'c' ){
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
  
  return(res)
  
}

#' @rdname recorded_io
#' @export
record_output <- function(call, quiet = FALSE){
  
  # Evaluate call first so that only valid calls are captured in record_files
  call
  
  # Capture call for output creation
  scall <- substitute(call)
  
  if ( class(scall) == 'call' & !is.vector(call) ){
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
#' information corresponds to an input or output file
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
      file1 <- eval(call[[2]])
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
  
  if ( class(io) == 'try-error' ){
    cli::cli_alert_warning(
      'Invalid {type} information: {file1}'
    )
  } else {
    assign(x = 'record_files', value = io, envir = ioenv)
    if ( nofile ){
      cli::cli_alert_warning(
        'First argument of inner call recorded: {file1}'
      )
    } else {
      if ( !quiet ){
        file1 <- suppressWarnings(normalizePath(file1))
        cli::cli_alert_info(
          '{snakecase::to_title_case(type)} file recorded: {file1}'
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
    
    if ( class(io) == 'try-error' ){
      cli::cli_alert_warning(
        'Invalid output information: {file2}'
      )
    } else {
      assign(x = 'record_files', value = io, envir = ioenv)
      if ( !quiet ){
        file2 <- suppressWarnings(normalizePath(file2))
        cli::cli_alert_info(
          'Output file(s) recorded: {file2}'
        )
      }
    }
  }
  
}

#' @rdname recorded_io
#' @export
recorded_io <- function(){
  
  if ( !exists('ioenv', mode = 'environment') ){
    stop('ioenv environment does not exist.')
  }
  
  record_files <- get(x = 'record_files', envir = ioenv)
  
  if ( nrow(record_files) == 0){
    stop('No recorded input or output files')
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
          sprintf('Invalid file: %s', x[3])
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
    sapply(ios, message)
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
  
}
