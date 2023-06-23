test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_erros <- function(table_name) {
    px_path <- create_px_file(table_name)
    output <- get_pxjob_file_path(table_name)

    pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
    expect_equal(0, pxjob_exit_code)

    # File is removed manually because pxjob_clean() cannot be used because the
    # exit code from pxjob64Win::pxjob is needed.
    file.remove(output)
  }

  expect_that_pxjob_runs_without_erros("BEXLTALL")
  expect_that_pxjob_runs_without_erros("BEXSTA")
  expect_that_pxjob_runs_without_erros("FOTEST")
})
