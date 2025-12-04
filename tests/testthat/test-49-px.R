test_that("px runs without errors (excel file)", {
  expect_runs_without_errors <- function(table_name) {
    px(input = get_metadata_path(table_name),
       data = get_data_path(table_name)
       )

    expect_true(TRUE)
  }

  expect_runs_without_errors("BEXLTALL")
  expect_runs_without_errors("BEXSTA")
  expect_runs_without_errors("FOTEST")
  expect_runs_without_errors("no_timeval_or_codes")
  expect_runs_without_errors("zero_heading")
  expect_runs_without_errors("zero_stub")
})

test_that("px runs without errors (PX-file)", {
  expect_runs_without_errors <- function(name) {
    px(input = get_px_file_path(name))

    expect_true(TRUE)
  }

  expect_runs_without_errors("BEXSTA_windows_1252")
  expect_runs_without_errors("SOXATI4")
  expect_runs_without_errors("TUX01")
  expect_runs_without_errors("CONTVARIABLE")
  expect_runs_without_errors("CONTVARIABLE_multiple_languages")
  expect_runs_without_errors("PRXPRISH")
  expect_runs_without_errors("multilingual_no_codes")
})

test_that("px runs without errors (data frame and rds path)", {
  expect_runs_without_errors <- function(name) {
    px(input = readRDS(get_data_path(name)))
    px(input = get_data_path(name))

    expect_true(TRUE)
  }

  expect_runs_without_errors("BEXLTALL")
  expect_runs_without_errors("BEXSTA_WITHOUT_TOTALS")
  expect_runs_without_errors("BEXSTA")
})

test_that("px runs without errors (data frame and parquet path)", {
  expect_runs_without_errors <- function(name) {
    px(input = arrow::read_parquet(get_data_path(name)))
    px(input = get_data_path(name))

    expect_true(TRUE)
  }

  expect_runs_without_errors("BEXSTA_parquet")
})

test_that("px can run on an Excel workbook without a 'Data' sheet", {
  px(input = get_metadata_path("BEXSTA"))

  expect_true(TRUE)
})

test_that("Minimal px object can be created without data", {
  x <- px()

  px_validate(x)

  expect_true(TRUE)
})


test_that("Validation can be turned off", {
  x0 <- px(women)

  x1 <- px_figures(x0, "fisk", validate = FALSE)

  expect_error(px_validate(x1), regexp = "variable-codes not in x")

  expect_true(TRUE)
})
