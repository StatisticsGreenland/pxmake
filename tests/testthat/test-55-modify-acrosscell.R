test_that('cellnote is modified and removed', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(cellnote(x), NULL)

  cellnote_df1 <- dplyr::tibble(`place of birth` = "*",
                                gender = "K",
                                time = "2018",
                                cellnote = "This is a cellnote"
                                )

  x1 <- cellnote(x, cellnote_df1)
  expect_identical(cellnote(x1), cellnote_df1)

  language_list <- c("en", "da", "kl")
  x_lang <- languages(x, language_list)

  expect_identical(cellnote(x_lang), NULL)

  cellnote_df2_lang <-
    tidyr::crossing(cellnote_df1, language = c("en", "da")) %>%
    dplyr::relocate(language, .before = "cellnote")

  x2_lang <- cellnote(x_lang, cellnote_df2_lang)

  expect_identical(cellnote(x2_lang), cellnote_df2_lang)

  cellnote_error1 <- dplyr::tibble(not_a_column = "fisk",
                                   `place of birth` = "*",
                                   gender = "K",
                                   time = "2018",
                                   cellnote = "This is a cellnote"
                                   )

  expect_error(cellnote(x, cellnote_error1), regexp = "invalid column")

  # Missing columns are set to '*'
  cellnote_df2 <- dplyr::tibble(gender = "K",
                                time = "2018",
                                cellnote = "This is a cellnote"
                                )

  x3 <- cellnote(x, cellnote_df2)
  expect_identical(cellnote(x3), cellnote_df1)


  # cellnotex
  expect_identical(cellnotex(x), NULL)

  cellnotex_df1 <- dplyr::tibble(cellnotex = "This is a cellnotex")

  x4 <- cellnotex(x, cellnotex_df1)

  expect_identical(cellnotex(x4), dplyr::tibble(`place of birth` = "*",
                                                gender = "*",
                                                time = "*",
                                                cellnotex = "This is a cellnotex"
                                                )
                   )
})
