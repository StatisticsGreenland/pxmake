test_that("Error data is specified when input is not .xlsx", {
  expect_data_input_error <- function(input, data) {
    expect_error(px(input = input, data = data),
                 regexp = "'data' can only be used if 'input' is an .xlsx file"
    )
  }

  expect_data_input_error(input = data.frame(),
                          data = data.frame()
  )

  expect_data_input_error(input = get_px_file_path("TUX01"),
                          data = data.frame()
  )
})

test_that("Error if 'Data' sheet is missing", {
  expect_error(px(input = get_metadata_path("BEXSTA"), data = NULL),
               regexp = "The sheet 'Data' is missing"
  )
})

test_that("Error if data is not NULL, a data frame, or .rds file", {
  expect_data_error <- function(data) {
    expect_error(px(input = get_metadata_path("FOTEST"), data = data),
                 regexp = "'data' has wrong format"
    )
  }

  expect_data_error(data = base::matrix())
  expect_data_error(data = list())
  expect_data_error(data = get_metadata_path("BEXSTA"))
})

test_that("Error if multiple time vars in variable", {
  expect_error(px(input = get_metadata_path("error_two_timevals")),
               regexp = "time"
               )
})

test_that("Error if add_totals is specified without xlsx metadata", {
  expect_error(pxmake_clean(temp_rds_file(),
                            temp_px_file(),
                            add_totals = c("place of birth", "gender")
                            ),
               regexp = "add_totals"
               )
})

test_that("Error wrong input", {
  expect_input_error <- function(input, data = NULL) {
    expect_error(px(input = input, data = data),
                 regexp = "'input' has wrong format"
                 )
  }

  expect_input_error(input = NULL)
  expect_input_error(input = list())
  expect_input_error(input = NULL, data = data.frame())
})

# micromake()

test_that("micromake arguments are validated", {
  expect_error(micromake(x = data.frame()),
               regexp = "px object"
               )

  expect_error(micromake(x = px(women), out_dir = 5),
               regexp = "character"
               )

  expect_error(micromake(x = px(women), out_dir = "not/a/directory"),
               regexp = "directory does not exist"
               )
})
