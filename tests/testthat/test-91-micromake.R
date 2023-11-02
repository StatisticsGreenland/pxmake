micromake_wrapper <- function(data_path, out_dir) {
  df <-
    readRDS(data_path) %>%
    dplyr::as_tibble() %>%
    dplyr::select(taar, civst, civdto, kirke)

  micro_metadata <- temp_xlsx_file()

  make_template(data_df = df,
                languages = c("en"),
                out_path = micro_metadata
                )

  micromake(data_df = df,
            metadata_path = micro_metadata,
            out_dir = out_dir
            )
}

test_that("micromake runs without errors and creates px files", {
  expect_that_micromake_runs_without_errors <- function(data_path) {
    out_dir <- temp_dir()

    micromake_wrapper(data_path = data_path,
                      out_dir = out_dir
                      )

    expect_true(length(list.files(out_dir)) > 0)
  }

  expect_that_micromake_runs_without_errors(get_data_path("micro"))
})

test_that("micromake creates valid px files", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_errors <- function(data_path) {
    out_dir <- temp_dir()

    micromake_wrapper(data_path = get_data_path("micro"),
                      out_dir = out_dir
                      )

    px_paths <- list.files(out_dir, full.names = TRUE)

    for (px_path in px_paths) {
      output <- temp_px_file()

      pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
      expect_equal(0, pxjob_exit_code)
    }
  }

  expect_that_pxjob_runs_without_errors(get_data_path("micro"))
})
