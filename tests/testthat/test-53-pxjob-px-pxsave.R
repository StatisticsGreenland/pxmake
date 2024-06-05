test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_errors <- function(x) {
    px_path <- temp_px_file()

    px_save(x, path = px_path)

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
  x1$cells2 <- get_base_cells2()

  expect_that_pxjob_runs_without_errors(x1)

  expect_that_pxjob_runs_without_errors(
    px(input = women) %>% px_languages(c("en", "sv"))
  )

  get_data_path('BEXSTA') %>%
    px() %>%
    px_language("en") %>%
    px_charset('ANSI') %>%
    px_creation_date('2020-01-01 10:00') %>%
    px_decimals("1") %>%
    px_heading("gender") %>%
    px_stub("place of birth") %>%
    px_last_updated("2020-01-01 10:00") %>%
    px_next_update("2020-01-01 10:00") %>%
    px_matrix("bexsta") %>%
    px_timeval("time") %>%
    px_valuenote(dplyr::tibble(`variable-code` = "gender",
                               code = "K",
                               valuenote = "Great value"
                               )
                 ) %>%
    px_valuenotex(dplyr::tibble(`variable-code` = "time",
                                code = "2020",
                                valuenotex = "What a great year"
                                )
                  ) %>%
    px_languages(c("en", "sv")) %>%
    px_note(dplyr::tibble(`variable-code` = "time",
                          language = c("en", "sv"),
                          note = c("English time note", "Svensk tidsnote")
                          )
            ) %>%
    px_notex("Mandatory note for entire table") %>%
    expect_that_pxjob_runs_without_errors()
})
