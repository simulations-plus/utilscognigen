filename_no_header <- file.path(tempdir(), "no-header.R")
filename_no_header_copy <- file.path(tempdir(), "no-header-copy.R")
cat(
  "rnorm(100)\n",
  file = filename_no_header
)

test_that("Rcopy adds header when no header exists", {
  
  expect_warning(
    object = Rcopy(
      from = filename_no_header,
      to = filename_no_header_copy
    ),
    regexp = "did not have a valid header"
  )
  
  # get_header returns FALSE if no header if found
  expect_false(
    object = get_header(filename_no_header)
  )
  
  expect_is(
    object = get_header(filename_no_header_copy),
    class = "character"
  )
  
  expect_identical(
    object = get_header_name(filename_no_header_copy),
    expected = filename_no_header_copy
  )
  
})

filename_header <- file.path(tempdir(), "header.R")
filename_header_copy <- file.path(tempdir(), "header-copy.R")
purpose <- "test purpose"
input_files <- c("file1.csv", "file2.csv")

Redit(
  filename_header,
  purpose = purpose,
  input_files = input_files,
  output_files = FALSE
)

test_that("Rcopy updates existing header", {
  
  Rcopy(
    from = filename_header,
    to = filename_header_copy
  )
  
  expect_length(
    object = get_header_history(filename_header),
    n = 1
  )
  
  expect_length(
    object = get_header_history(filename_header_copy),
    n = 2
  )
  
  expect_identical(
    object = get_header_name(filename_header_copy),
    expected = filename_header_copy
  )
  
  expect_is(
    object = get_header_history(filename_header_copy),
    class = "character"
  )
  
  expect_identical(
    object = unique(get_header_version(filename_header_copy)),
    expected = paste0(R.version$major, ".", R.version$minor)
  )
  
  expect_match(
    object = get_header_copyright(filename_header_copy),
    regexp = "^Copyright"
  )
  
  expect_identical(
    object = get_header_purpose(filename_header_copy),
    expected = purpose
  )
  
  expect_identical(
    object = get_header_input_files(filename_header_copy),
    expected = input_files
  )
  
  expect_null(
    object = get_header_output_files(filename_header_copy)
  )
  
  
})
