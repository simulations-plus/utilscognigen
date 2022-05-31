
# R files -----------------------------------------------------------------

filename <- file.path(tempdir(), "test-headers.R")
purpose <- "test purpose"
input_files <- c("file1.csv", "file2.csv")

Redit(
  filename,
  purpose = purpose,
  input_files = input_files,
  output_files = FALSE
)

test_that("header retrievers work", {
  
  expect_identical(
    object = get_header_name(filename),
    expected = filename
  )
  
  expect_is(
    object = get_header_timestamp(filename),
    class = "character"
  )
  
  expect_identical(
    object = get_header_version(filename),
    expected = paste0(R.version$major, ".", R.version$minor)
  )
  
  expect_match(
    object = get_header_copyright(filename),
    regexp = "^Copyright"
  )
  
  expect_identical(
    object = get_header_purpose(filename),
    expected = purpose
  )
  
  expect_identical(
    object = get_header_input_files(filename),
    expected = input_files
  )
  
  expect_null(
    object = get_header_output_files(filename)
  )
  
})


# Rmd files ---------------------------------------------------------------

filename <- file.path(tempdir(), "test-headers.Rmd")
purpose <- "test purpose"
input_files <- c("file1.csv", "file2.csv")

Redit(
  filename,
  purpose = purpose,
  input_files = input_files,
  output_files = FALSE
)

test_that("header retrievers work", {
  
  expect_identical(
    object = get_header_name(filename),
    expected = filename
  )
  
  expect_is(
    object = get_header_timestamp(filename),
    class = "character"
  )
  
  expect_identical(
    object = get_header_version(filename),
    expected = paste0(R.version$major, ".", R.version$minor)
  )
  
  expect_match(
    object = get_header_copyright(filename),
    regexp = "^Copyright"
  )
  
  expect_identical(
    object = get_header_purpose(filename),
    expected = purpose
  )
  
  expect_identical(
    object = get_header_input_files(filename),
    expected = input_files
  )
  
  expect_null(
    object = get_header_output_files(filename)
  )
  
})

