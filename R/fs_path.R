#' Build file paths based on Cognigen's file system and directory structure
#'
#' @md
#'
#' @name fs_path
#' 
#' @description 
#' These functions are only intended to be used at Cognigen.
#' 
#' See
#' \href{https://wiki.cognigencorp.com/display/qms/QMS-1.6+Standard+Directory+Structure}{Wiki
#' QMS 1.6 Standard Directory Structure}.
#'
#' @param sponsor,drug,project_number \code{character} vectors of directory
#'   patterns or names. Wildcard expansion (also known as 'globbing') is
#'   supported. See \code{\link[base]{Sys.glob}}.
#' @param path \code{character} vector of file or directory paths that will be
#'   truncated to the project number, drug, or sponsor level. Ignored in
#'   \code{path_*} functions when the first argument is provided (i.e.,
#'   \code{path} is ignored if the \code{sponsor} argument is provided to
#'   \code{path_sponsor()}).
#'
#' @details 
#' ## \code{path_*}
#' 
#' \code{path_sponsor()} returns the path to sponsor directories. 
#' * When only the \code{path} argument is set (including the default,
#' \code{path = "."}), returns one sponsor directory corresponding to each
#' \code{path}.
#' * When the \code{sponsor} argument is provided, \code{path} is ignored and
#' all sponsor directories matching any element of \code{sponsor} are returned.
#'   
#' \code{path_drug()} returns the path to drug directories, which are one
#' directory below sponsor directories.
#' * When only the \code{path} argument is set (including the default,
#' \code{path = "."}), returns one drug directory corresponding to each
#' \code{path}.
#' * When the \code{drug} argument is provided, \code{path} is ignored and all
#' drug directories matching any element of \code{drug} are returned. Further
#' subsetting of drug directories can be done by providing \code{sponsor}.
#'   
#' \code{path_project()} returns the path to project directories, which are one
#' directory below drug directories.
#' * When only the \code{path} argument is set (including the default,
#' \code{path = "."}), returns one project directory corresponding to each
#' \code{path}.
#' * When the \code{project_number} argument is provided, \code{path} is ignored
#' and all project directories matching \code{project_number} are returned.
#' Further subsetting of project directories can be done by providing
#' \code{drug} and/or \code{sponsor}. Resulting directories will match at least
#' one element of both \code{drug} and \code{sponsor}.
#'
#' ## \code{browse_project_*}
#' \code{browse_project_email()} opens email aliases in Outlook on the web in
#' a new browser tab.
#' * When called without any arguments, opens the email alias corresponding to
#' the working directory.
#' * When one or more paths are provided to \code{path}, opens all corresponding
#' project email aliases.
#' * Email aliases that do not exist or you do not have access to might open,
#' but not display email content.
#'   
#' \code{browse_project_sharepoint()} opens internal SharePoint sites in a new
#' browser tab.
#' * When called without any arguments, opens the SharePoint site corresponding
#' to the working directory.
#' * When one or more paths are provided to \code{path}, opens all corresponding
#' SharePoint sites.
#' * SharePoint sites that do not exist are expected to be skipped.
#'   
#'
#' @return For \code{path_*} functions, the path(s) to the sponsor, drug, or
#'   project directory corresponding to either: the \code{path} of a file or
#'   directory; or the name(s) of the sponsor, drug, or project number.
#'
#'   For \code{browse_project_*} functions, invisibly returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' # the below examples reference a test directory on Cognigen's file system.
#' cd("/misc/dceuticals/doloxan/009002/")
#'
#' # the default behavior is to return the path to the sponsor, drug, or project.
#' path_sponsor()
#' path_drug()
#' path_project()
#'
#' # or open the resource for the corresponding project.
#' # note that these will not produce desired results with the test dceuticals
#' # directory because the project alias and SharePoint site do not exist.
#' browse_project_email()
#' browse_project_sharepoint()
#'
#' # the below examples do not rely on the working directory
#' cd("~")
#'
#' # use the pattern "*" to match any sponsor, drug, or project number.
#'
#' # get paths to all sponsor directories:
#' path_sponsor("*")
#'
#' # get paths to sponsor directories that begin with "bio":
#' path_sponsor("bio*")
#'
#' # get paths to drug directories that end with "umab":
#' path_drug("*umab")
#'
#' # get paths to project directories that begin with "20" for drugs that end
#' # with "umab" or start with "ab":
#' path_project("20*", drug = c("*umab", "ab*"))
#'
#' # open email aliases for all projects where the project number starts with
#' # "006" and ends with "58"
#' browse_project_email(path_project("006*58"))
#'
#' }
NULL
#> NULL

# directory names that will not be considered as sponsors
.drop_sponsor_dirs <- c(
  ".zfs", "apps", "archive", "cognigen", "doc", "gridengine", "Linux"
)

# directory names that will not be considered as drugs
.drop_drug_dirs <- c(
  "client_standards", "concepts", "consult_gen", "kiwi", "legal", 
  "annualrpt2001", "admin", "clinical_trials", "current", "consulting", "bdgt", 
  "data", "doc", "email", "let", "ltr", "minutes", "nm", "perspective", 
  "presentation", "presentations", "Presentations",  "Projman", "projman", 
  "Proposals-Schedule", "proposal", "rpt", "sas", "sponsordoc", "training"
)

#' @rdname fs_path
#' @export
path_sponsor <- function(sponsor, path = ".") {
  
  require_cognigen()
  
  # if no sponsor is provided, return sponsor directory path
  if(missing(sponsor)) {
    path <- fs::path_real(path)
    paths <- vapply(path, function(p) {
      
      ps <- fs::path_split(p)[[1]]
      
      if(length(ps) < 3) {
        cli::cli_abort(
          "{.var path} does not include enough directory levels to be a sponsor directory: {.file {p}}"
        )
      }
      
      if(ps[[2]] != "misc") {
        cli::cli_abort(
          "{.var path} is not in {.file /misc}: {.file {p}}",
          call = NULL
        )
      }
      
      file.path(ps[[1]], ps[[2]], ps[[3]])
    },
    FUN.VALUE = character(1L))
    
    return(fs::path_real(unique(paths)))
    
  }
  
  # identify all matching sponsors
  assertthat::assert_that(
    is.character(sponsor)
  )
  
  paths <- Sys.glob(file.path("/misc", sponsor))
  paths <- paths[dir.exists(paths)]
  paths <- fs::path_real(paths)
  paths <- paths[!basename(paths) %in% .drop_sponsor_dirs]
  
  fs::path_real(unique(paths))
  
}


#' @rdname fs_path
#' @export
path_drug <- function(drug, sponsor = "*", path = ".") {
  
  require_cognigen()
  
  # if no drug is provided, return path to drug directory
  if(missing(drug)) {
    path <- fs::path_real(path)
    paths <- vapply(path, function(p) {
      
      ps <- fs::path_split(p)[[1]]
      
      if(length(ps) < 4) {
        cli::cli_abort(
          "{.var path} does not include enough directory levels to be a drug directory: {.file {p}}"
        )
      }
      
      if(ps[[2]] != "misc") {
        cli::cli_abort(
          "{.var path} is not in {.file /misc}: {.file {p}}",
          call = NULL
        )
      }
      
      file.path(ps[[1]], ps[[2]], ps[[3]], ps[[4]])
    },
    FUN.VALUE = character(1L))
    
    return(fs::path_real(unique(paths)))
    
  }
  
  # identify all matching sponsor/drug combinations
  assertthat::assert_that(
    is.character(drug)
  )
  
  paths <- Sys.glob(file.path("/misc", sponsor, drug))
  paths <- paths[dir.exists(paths)]
  paths <- fs::path_real(paths)
  
  paths <- paths[!basename(paths) %in% .drop_drug_dirs]
  paths <- paths[!basename(dirname(paths)) %in% .drop_sponsor_dirs]
  
  fs::path_real(unique(paths))
  
}


#' @rdname fs_path
#' @export
path_project <- function(project_number, drug = "*", sponsor = "*", path = ".") {
  
  require_cognigen()
  
  # if no project_number is provided, return path to project directory
  if(missing(project_number)) {
    path <- fs::path_real(path)
    paths <- vapply(path, function(p) {
      
      ps <- fs::path_split(p)[[1]]
      
      if(length(ps) < 5) {
        cli::cli_abort(
          "{.var path} does not include enough directory levels to be a project directory: {.file {p}}"
        )
      }
      
      if(ps[[2]] != "misc") {
        cli::cli_abort(
          "{.var path} is not in {.file /misc}: {.file {p}}",
          call = NULL
        )
      }
      
      file.path(ps[[1]], ps[[2]], ps[[3]], ps[[4]], ps[[5]])
    },
    FUN.VALUE = character(1L))
    
    return(fs::path_real(unique(paths)))
    
  }
  
  assertthat::assert_that(
    is.character(project_number) || is.numeric(project_number)
  )
  
  leading_zero_n <- 6 - nchar(project_number)
  
  project_number <- mapply(
    FUN = function(pn, lzn) {
      if(grepl("\\*", pn)) {
        pn
      } else if(lzn > 0) {
        paste0(paste0(replicate(n = lzn, expr = 0), collapse = ""), pn)
      } else {
        pn
      }
    },
    pn = project_number,
    lzn = leading_zero_n
  )
  
  paths <- Sys.glob(file.path("/misc", sponsor, drug, project_number))
  paths <- paths[dir.exists(paths)]
  paths <- fs::path_real(paths)
  
  paths <- paths[!basename(dirname(paths)) %in% .drop_drug_dirs]
  paths <- paths[!basename(dirname(dirname(paths))) %in% .drop_sponsor_dirs]
  
  # remove directories that do not include 6 consecutive digits
  paths <- paths[grepl("\\d{6}", basename(paths))]
  
  paths <- paths[!basename(dirname(paths)) %in% .drop_drug_dirs]
  paths <- paths[!basename(dirname(dirname(paths))) %in% .drop_sponsor_dirs]
  
  paths <- paths[dir.exists(paths)]
  
  fs::path_real(unique(paths))
  
}

# helper function to make an alias of the form sponsor.project_number
mk_alias <- function(path = ".") {
  
  require_cognigen()
  
  project_path <- path_project(path = path)
  
  # paste together the sponsor name with the project_number
  paste0(basename(dirname(dirname(project_path))), ".", basename(project_path))
  
}

#' @rdname fs_path
#' @export
browse_project_email <- function(path = ".") {
  
  require_cognigen()
  
  outlook_groups_url <- getOption("utilscognigen.outlook_groups_url")
  if(is.null(outlook_groups_url)) {
    cli::cli_abort("The {.arg utilscognigen.outlook_groups_url} option is not set.")
  }
  
  alias <- mk_alias(path)
  
  url <- file.path(outlook_groups_url, alias, "email")
  
  for(u in url) {
    utils::browseURL(u)
  }
  
  invisible(NULL)
  
}

#' @rdname fs_path
#' @export
browse_project_sharepoint <- function(path = ".") {
  
  require_cognigen()
  
  sharepoint_sites_url <- getOption("utilscognigen.sharepoint_sites_url")
  if(is.null(sharepoint_sites_url)) {
    cli::cli_abort("The {.arg utilscognigen.sharepoint_sites_url} option is not set.")
  }
  
  alias <- mk_alias(path)
  
  url <- file.path(sharepoint_sites_url, alias)
  
  for(u in url) {
    # binary body indicates that the page does not exist
    if(is.raw(httr::content(httr::GET(u)))) {
      message("Skipping ", basename(u), " because the page does not exist.")
    } else {
      utils::browseURL(u)
    }
  }
  
  invisible(NULL)
  
}
