test_that('VALUENOTE(X) is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  x_lang <- px_languages(x, c('dk', 'kl'))

  expect_identical(px_valuenote(x), NULL)
  expect_identical(px_valuenote(x_lang), NULL)

  valuenote_df1 <- dplyr::tibble(`variable-code` = 'gender',
                                 code = 'K',
                                 language = 'kl',
                                 valuenote = 'A valuenote'
                                 )

  x2 <- px_valuenote(x_lang, valuenote_df1)
  expect_identical(px_valuenote(x2), valuenote_df1)

  valuenote_df2 <- dplyr::tibble(`variable-code` = 'place of birth',
                                 code = 'N',
                                 valuenote = 'Second valuenote'
                                 )

  x3 <- px_valuenote(x_lang, valuenote_df2)

  valuenote_df2_expect <- tidyr::crossing(valuenote_df2,
                                          language = c('kl', 'dk')
                                          ) %>%
    dplyr::relocate(valuenote, .after = last_col())

  expect_identical(px_valuenote(x3), valuenote_df2_expect)

  x4 <- px_valuenote(x_lang, NULL)
  expect_identical(px_valuenote(x4), NULL)

  valuenote_df3 <- valuenote_df2
  valuenote_df3_expect <-
    tidyr::crossing(valuenote_df2, language = NA_character_) %>%
    dplyr::relocate(valuenote, .after = last_col())

  x5 <- px_valuenote(x, valuenote_df3)

  expect_identical(px_valuenote(x5), valuenote_df3_expect)

  expect_error(px_valuenote(x, valuenote_df1), regex = 'LANGUAGE')


  valuenotex_df1 <- dplyr::rename(valuenote_df1, valuenotex = valuenote)

  x6 <- px_valuenotex(x_lang, valuenotex_df1)

  expect_identical(px_valuenotex(x6), valuenotex_df1)
})
