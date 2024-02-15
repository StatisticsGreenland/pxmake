test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_erros <- function(x) {
    px_path <- temp_px_file()

    pxsave(x, path = px_path)

    output <- get_pxjob_file_path(rlang::hash(x))

    pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
    expect_equal(0, pxjob_exit_code)

    # File is removed manually because pxjob_clean() cannot be used because the
    # exit code from pxjob64Win::pxjob is needed.
    file.remove(output)
  }

  expect_that_pxjob_runs_without_erros(px(input = get_metadata_path("FOTEST")))
  expect_that_pxjob_runs_without_erros(px(input = get_px_file_path("TUX01")))
  expect_that_pxjob_runs_without_erros(px(input = readRDS(get_data_path("BEXSTA"))))
})
