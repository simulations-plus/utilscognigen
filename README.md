# utilscognigen

# Overview

`utilscognigen` provides utility functions for R and RStudio at Cognigen. This package facilitates creation, execution, and review of R files. This package also provides an interface for interacting with the Cognigen file system and sponsor directory structure.

Load it with:
```r
library(utilscognigen)
```

# Functionality

## Creating R Files

- Create, update, and open R files with QMS approved headers with `Redit()` and `Rcopy()`. 
- Information can be retrieved from existing headers using the `get_header_*` family of functions.

## Executing R Files

- Execute R scripts with `rcb()`. This will produce a log file (.Rout).
- Render R Markdown with `render()`. This produces an R script to render the Rmd file, which also results in an associated .Rout file.

## Recording Input and Output Files

- `record_input()` and `record_output()` store IO information in a dedicated environment.
- Summarize the collected IO information with `recorded_io()`.

## Reviewing R and Rout Files

- Review warnings and errors with `scanlogs()`, which is run automatically after `rcb()` and `render()`.
- Review recorded input and output files of an executed R file with `get_recorded_io()`.
- Check if an R file meets Cognigen standards with `check_standards()`.
- Review the entire .Rout file (as well as other logfiles) with `logfile()`.

## Interacting with the Cognigen File System

- Many functions require a `path` argument in this package. Generally, the open file in RStudio will be used when no other path is provided. See `get_source_file()`.
- Convert file paths between their Unix and Windows representations with `path_to_unix()` and `path_to_windows()`.
  - Also see the Toggle File Path Selection RStudio Addin.
- Navigate Outlook and SharePoint project resources with `browse_project_email()` and `browse_project_sharepoint()`.
- Build file paths based on Cognigen's file system and directory structure with `path_sponsor()`, `path_drug()`, and `path_project()`.
- Interact with the Cognigen shared-code repository and quickly identify and copy files with `browse_shared_code()`, `list_files_shared_code()`, and `Rcopy_shared_code()`.
- Copy snippets (including shared-code snippets) with `set_rstudio_snippets()`.
- Interact with Cognigen's zfs snapshots with `file_snapshots()`, `diff_snapshot()`, and `restore_snapshot()`.
