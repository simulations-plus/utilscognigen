
test_that(
  "Setup",
  {
    dat <- data.frame(a = 1, b = 'a')
    dat2 <- data.frame(a = 2, b = 'b')
    write.csv(
      dat,
      file = file.path(tempdir(), 'dat.csv'),
      row.names = FALSE,
      quote = FALSE
    )
    write.table(
      dat,
      file = file.path(tempdir(), 'dat.tbl'),
      row.names = FALSE,
      quote = FALSE
    )
    haven::write_xpt(
      dat,
      path = file.path(tempdir(), 'dat.xpt'),
      version = 5
    )
    openxlsx::write.xlsx(
      dat,
      file = file.path(tempdir(), 'dat.xlsx'),
      overwrite = TRUE
    )
    save(dat, dat2, file = file.path(tempdir(), 'dat.Rda'))
    write('some text', file = file.path(tempdir(), 'dat.txt'))
    rm(dat)
    succeed()
  }
)

test_that(
  "record_input: display of information message", 
  {
    expect_message(
      { record_input('a')},
      'file recorded'
    )
  }
)

test_that(
  "record_input: hide information message", 
  {
    expect_silent(
      { record_input('b', quiet = TRUE) }
    )
  }
)

test_that(
  "record_output: display of information message", 
  {
    expect_message(
      { record_output('c') },
      'file recorded'
    )
  }
)

test_that(
  "record_output: hide information message", 
  {
    expect_silent(
      { record_output('d', quiet = TRUE) }
    )
  }
)

test_that(
  "record_input: output type when file paths are explicitly provided", 
  {
    expect_type(
      { record_input(c('e', 'f')) },
      'character'
    )
  }
)

test_that(
  "record_input: output length when file paths are explicitly provided", 
  {
    expect_length(
      { record_input(c('g', 'h', 'i')) },
      3
    )
  }
)

test_that(
  "recorded_io: class of record_files in ioenv environment", 
  {
    expect_s3_class(
      { get(x = 'record_files', envir = ioenv) },
      'data.frame'
    )
  }
)

test_that(
  "recorded_io: dimensions of record_files in ioenv environment", 
  {
    expect_identical(
      { dim(get(x = 'record_files', envir = ioenv)) },
      c(9L, 3L)
    )
  }
)

test_that(
  "recorded_io: invalid files", 
  {
    expect_message(
      { recorded_io() },
      'Invalid file'
    )
  }
)

test_that(
  "clear_recorded_io: invisible", 
  {
    expect_invisible(
      { clear_recorded_io() }
    )
  }
)


test_that(
  "clear_recorded_io: clear recorded information", 
  {
    expect_identical(
      { dim(get(x = 'record_files', envir = ioenv)) },
      c(0L, 3L)
    )
  }
)

test_that(
  "record_input: output class when a function call (read.csv) provided", 
  {
    expect_s3_class(
      { 
        record_input(
          read.csv(
            file = file.path(tempdir(), 'dat.csv'),
            header = TRUE
          )
        )
      },
      'data.frame'
    )
  }
)

test_that(
  "record_input: output class when a function call (read.table) provided", 
  {
    expect_s3_class(
      { 
        record_input(
          read.table(
            file = file.path(tempdir(), 'dat.tbl'),
            header = TRUE
          )
        )
      },
      'data.frame'
    )
  }
)

test_that(
  "record_input: output class when a function call (load) provided", 
  {
    expect_s3_class(
      { 
        record_input(
          load(
            file = file.path(tempdir(), 'dat.Rda'),
          )
        )
        dat
      },
      'data.frame'
    )
  }
)

test_that(
  "record_input: output type when a function call (readChar) provided ", 
  {
    expect_type(
      record_input(
        readChar(con = file.path(tempdir(), 'dat.txt'), nchars = file.info(file.path(tempdir(), 'dat.txt'))$size)
      ),
      'character'
    )
  }
)

test_that(
  "record_input: output type when a function call (haven::read_xpt) provided ", 
  {
    expect_s3_class(
      { 
        record_input(
          haven::read_xpt( file = file.path(tempdir(), 'dat.xpt') )
        )
      },
      'data.frame'
    )
  }
)

test_that(
  "record_input: output type when a function call (read) provided", 
  {
    expect_s3_class(
      { 
        record_input(
          readxl::read_xlsx( path = file.path(tempdir(), 'dat.xlsx') )
        )
      },
      'data.frame'
    )
  }
)

test_that(
  "recorded_io: messages report input files", 
  {
    expect_message(
      { recorded_io() },
      '[*][*][[]input[]]'
    )
  }
)

test_that(
  "recorded_io: recorded file info",
  {
    expect_identical(
      {
        get(x = 'record_files', envir = ioenv)$path
      },
      file.path(
        tempdir(),
        c('dat.csv', 'dat.tbl', 'dat.Rda', 'dat.txt', 'dat.xpt', 'dat.xlsx')
      )
    )
  }
)


test_that(
  "record_output: create 1 file (write) and record information",
  {
    expect_identical(
      {
        clear_recorded_io()
        record_output(
          write('junk', file = file.path(tempdir(), 'junk'))
        )
        dim(get(x = 'record_files', envir = ioenv))
      },
      c(1L, 3L)
    )
    
  }
)

test_that(
  "record_output: output type when a function call (haven::write_xpt) provided ", 
  {
    expect_identical(
      {
        clear_recorded_io()
        record_input(
          haven::write_xpt(
            data = data.frame(a = 1, b = 2),
            path = file.path(tempdir(), 'junk.xpt') 
          )
        )
        dim(get(x = 'record_files', envir = ioenv))
      },
      c(1L, 3L)
    )
  }
)

test_that(
  "record_output: create multiple files (png) and record information",
  {
    expect_identical(
      {
        clear_recorded_io()
        record_output(
          png(
            file = file.path(tempdir(), '/output_%03d.png'),
            width = 2, height = 2, units = 'in', res = 300
          )
        )
        plot(1, 1) ; plot(2, 2) ; plot(3, 3)
        dev.off()
        dim(get(x = 'record_files', envir = ioenv))
      },
      c(1L, 3L)
    )
  }
)

test_that(
  "get_recorded_io: retrieve input and output files",
  {
    expect_identical(
      {
        file.create(file.path(tempdir(), "input1.csv"))
        file.create(file.path(tempdir(), "input2.csv"))
        program_content <- c(
          "library(utilscognigen)",
          "record_input('input1.csv')",
          "record_input('input2.csv')",
          "recorded_io()"
        )
        
        program_name <- file.path(tempdir(), "get-recorded-io-test.R")
        
        cat(
          program_content,
          file = program_name,
          sep = "\n"
        )
        
        rcb(program_name, as_job = FALSE)
        
        Sys.sleep(10)
        result <- get_recorded_io(program_name)
        print(result$input_files)
        result
        
      },
      list(
        input_files = normalizePath(file.path(tempdir(), c("input1.csv", "input2.csv"))),
        output_files = character()
      )
    )
  }
)
