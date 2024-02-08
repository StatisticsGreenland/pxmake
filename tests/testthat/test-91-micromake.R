test_that("micromake creates valid px files", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_errors <- function(px) {
    out_dir <- temp_dir()

    micromake(px, out_dir = out_dir)

    px_paths <- list.files(out_dir, full.names = TRUE)

    for (px_path in px_paths) {
      output <- temp_px_file()
      pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)

      expect_equal(0, pxjob_exit_code, info = px_path)
    }
  }

  get_data_path("micro") %>%
    readRDS() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(sidedoer = stringr::str_trim(sidedoer),
                  sidedoer = dplyr::na_if(sidedoer, ""),
                  pnr = NA
                  ) %>%
    px() %>%
    timeval("taar") %>%
    expect_that_pxjob_runs_without_errors()
})
