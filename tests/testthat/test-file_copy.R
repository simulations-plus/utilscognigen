test_that("file_copy works", {
  
  tmp <- tempfile()
  file.create(tmp)
  
  # copy file to file
  expect_true(
    file_copy(from = tmp, to = tempfile())
  )
  
  # copy file to directory
  expect_true(
    file_copy(from = tmp, to = file.path(tempdir(), "supertmp"))
  )
  
})


test_that("file_copy copies a file with a space", {

  file_with_space <- file.path(tempdir(), "temp file with_space.txt")
  file.create(file_with_space)

  new_file_with_space <- file.path(tempdir(), "new temp file with_space.txt")

  expect_true(
    file_copy(from = file_with_space, to = new_file_with_space)
  )

})
