test_that('cellnote is modified and removed', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(px_cellnote(x), NULL)

  cellnote_df1 <- dplyr::tibble(`place of birth` = "*",
                                gender = "K",
                                time = "2018",
                                cellnote = "This is a cellnote"
                                )

  x1 <- px_cellnote(x, cellnote_df1)
  expect_identical(px_cellnote(x1), cellnote_df1)

  # Astrixs are added for variables that aren't supplied
  cellnote_df2 <- dplyr::select(cellnote_df1, -`place of birth`)
  x2 <- px_cellnote(x, cellnote_df2)
  expect_identical(px_cellnote(x2), cellnote_df1)

  # Changing languages adds cellnote for each language
  language_list <- c("da", "en", "kl")

  x1_lang <- px_languages(x1, language_list)

  cellnote_df2_lang <-
    tidyr::crossing(cellnote_df1, language = language_list) %>%
    dplyr::relocate(language, .before = "cellnote")

  expect_identical(px_cellnote(x1_lang), cellnote_df2_lang)

  # Set cellnote for multiple languages
  x2_lang <-
    x %>%
    px_languages(language_list) %>%
    px_cellnote(cellnote_df2_lang)

  expect_identical(px_cellnote(x2_lang), cellnote_df2_lang)

  # error if column is not in the data
  cellnote_error1 <- dplyr::tibble(not_a_column = "fisk",
                                   `place of birth` = "*",
                                   gender = "K",
                                   time = "2018",
                                   cellnote = "This is a cellnote"
                                   )

  expect_error(px_cellnote(x, cellnote_error1), regexp = "invalid column")

  # cellnotex
  expect_identical(px_cellnotex(x), NULL)

  cellnotex_df1 <- cellnote_df1 %>% dplyr::rename(cellnotex = cellnote)

  x4 <- px_cellnote(x, dplyr::filter(cellnote_df1, FALSE))

  expect_identical(px_cellnote(x4), px_cellnote(x)) # cellnote which no rows should not modify

  x5 <- px_cellnotex(x, cellnotex_df1)

  expect_identical(px_cellnotex(x5), cellnotex_df1)

  x6 <- px_cellnotex(x, NULL)

  expect_identical(px_cellnotex(x6), NULL)
})
