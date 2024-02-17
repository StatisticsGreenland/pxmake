test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_errors <- function(x) {
    px_path <- temp_px_file()

    pxsave(x, path = px_path)

    output <- get_pxjob_file_path(rlang::hash(x))

    pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
    expect_equal(0, pxjob_exit_code)

    # File is removed manually because pxjob_clean() cannot be used because the
    # exit code from pxjob64Win::pxjob is needed.
    file.remove(output)
  }

  expect_that_pxjob_runs_without_errors(px(input = get_metadata_path("FOTEST")))
  expect_that_pxjob_runs_without_errors(px(input = get_px_file_path("TUX01")))
  expect_that_pxjob_runs_without_errors(px(input = get_data_path("BEXSTA")))

  x1 <- px(input = women)
  x1$codelists2 <- get_base_codelists2()

  expect_that_pxjob_runs_without_errors(x1)

  expect_that_pxjob_runs_without_errors(
    px(input = women) %>% languages(c("en", "sv"))
  )

  get_data_path('BEXSTA') %>%
    px() %>%
    language("en") %>%
    charset('ANSI') %>%
    creation_date('2020-01-01 10:00') %>%
    decimals("1") %>%
    heading("gender") %>%
    stub("place of birth") %>%
    last_updated("2020-01-01 10:00") %>%
    next_update("2020-01-01 10:00") %>%
    matrix("bexsta") %>%
    timeval("time") %>%
    valuenote(dplyr::tibble(`variable-code` = "gender",
                            code = "K",
                            valuenote = "Great value"
                            )
              ) %>%
    valuenotex(dplyr::tibble(`variable-code` = "time",
                             code = "2020",
                             valuenotex = "What a great year"
                             )
               ) %>%
    languages(c("en", "sv")) %>%
    expect_that_pxjob_runs_without_errors()
})
