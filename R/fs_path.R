#' Build file paths based on Cognigen's file system and directory structure.
#' 
#' These functions are only intended to be used at Cognigen.
#'
#' @name fs_path
#'
#' @param sponsor,drug,project_number \code{character} vectors of directory
#'   patterns or names. Wildcard expansion (also known as 'globbing') is
#'   supported.
#' @param path \code{character} vector of file or directory paths that will be
#'   truncated to the project number, drug, or sponsor level. Ignored when the
#'   first argument is provided.
#' 
#' @return 
#' The \code{path_*} functions return the path to the sponsor, drug, or project
#' directory corresponding to either: the \code{path} of a file or directory; or
#' the name of the sponsor, drug, or project number.
#' 
#' The \code{browse_project_*} functions open the corresponding project's email
#' alias or SharePoint site in a new browser tab.
#' 
#' \code{path_sponsor()} returns the path to the sponsor directory.
#'
#' \code{path_drug()} returns the path to the drug directory.
#' 
#' \code{path_project()} returns the path to the project directory.
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
  ".zfs", "apps", "archive", "cognigen", "doc", "gridengine"
)

# directory names that will not be considered as drugs
.drop_drug_dirs <- c(
  "client_standards", "concepts", "consult_gen", "kiwi", "legal", 
  # directories that should not be at this level, but were found.
  "admin", "consulting", "let", "ltr", "presentation", "Presentations", 
  "Projman", "proposal", "sponsordoc", "training"
)

#' @rdname fs_path
#' @export
path_sponsor <- function(sponsor, path = ".") {
  
  require_cognigen()
  
  # if no sponsor is provided, subset path to sponsor
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
  
  assertthat::assert_that(
    is.character(sponsor)
  )
  
  paths <- fs::path_real(Sys.glob(file.path("/misc", sponsor)))
  
  paths <- paths[!basename(paths) %in% .drop_sponsor_dirs]
  
  paths <- paths[dir.exists(paths)]
  
  fs::path_real(unique(paths))
  
}


#' @rdname fs_path
#' @export
path_drug <- function(drug, sponsor = "*", path = ".") {
  
  require_cognigen()
  
  # if no drug is provided, subset path to drug
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
  
  assertthat::assert_that(
    is.character(drug)
  )
  
  paths <- fs::path_real(Sys.glob(file.path("/misc", sponsor, drug)))
  
  paths <- paths[!basename(paths) %in% .drop_drug_dirs]
  paths <- paths[!basename(dirname(paths)) %in% .drop_sponsor_dirs]
  
  paths <- paths[dir.exists(paths)]
  
  fs::path_real(unique(paths))
  
}


#' @rdname fs_path
#' @export
path_project <- function(project_number, drug = "*", sponsor = "*", path = ".") {
  
  require_cognigen()
  
  # if no project_number is provided, subset path to project_number
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
  
  paths <- fs::path_real(Sys.glob(file.path("/misc", sponsor, drug, project_number)))
  
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
  
  alias <- mk_alias(path)
  
  url <- file.path("https://outlook.office.com/mail/group/cognigencorp.com", alias, "email")
  
  for(u in url) {
    browseURL(u)
  }
  
  invisible(NULL)
  
}

#' @rdname fs_path
#' @export
browse_project_sharepoint <- function(path = ".") {
  
  require_cognigen()
  
  alias <- mk_alias(path)
  
  url <- file.path("https://simulationsplus.sharepoint.com/sites", alias)
  
  for(u in url) {
    # binary body indicates that the page does not exist
    if(is.raw(httr::content(httr::GET(u)))) {
      message("Skipping ", basename(u), " because the page does not exist.")
    } else {
      browseURL(u)
    }
  }
  
  invisible(NULL)
  
}
