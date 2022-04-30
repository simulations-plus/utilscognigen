olddir <- getwd()

test_that(
  'cd changes working directory', 
  {
    cd(tempdir())
    expect_equal(getwd(), tempdir())
  }
)

setwd(olddir)

test_that(
  'pwd return current directory',
  {
    expect_equal(pwd(), getwd())
  }
)
