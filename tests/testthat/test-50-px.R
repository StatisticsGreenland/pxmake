test_that("px runs without errors", {
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
