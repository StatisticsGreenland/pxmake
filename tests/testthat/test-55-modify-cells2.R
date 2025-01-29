test_that('px_valuenote() and px_valuenotex()', {
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

test_that('px_values()', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  x_lang <- px_languages(x, c('dk', 'kl'))

  x1 <-
    x %>%
    px_values(dplyr::tibble(`variable-code` = 'gender',
                            code = 'K',
                            value = 'Kvinde'
                            )
              )

  expect <-
    dplyr::tribble(
     ~`variable-code`,  ~code,            ~language,   ~value,
     "place of birth",    "N",        NA_character_,      "N",
     "place of birth",    "S",        NA_character_,      "S",
     "place of birth",    "T",        NA_character_,      "T",
             "gender",    "K",        NA_character_, "Kvinde",
             "gender",    "M",        NA_character_,      "M",
             "gender",    "T",        NA_character_,      "T",
               "time", "2018",        NA_character_,   "2018",
               "time", "2019",        NA_character_,   "2019",
               "time", "2020",        NA_character_,   "2020",
               "time", "2021",        NA_character_,   "2021",
               "time", "2022",        NA_character_,   "2022"
                  )

  expect_identical(px_values(x1), expect)

  values_df <-
    dplyr::tribble( ~`variable-code`,  ~code, ~language,           ~value,
                              "time", "2018",      "dk",        "år 2018",
                              "time", "2018",      "kl",  "ukiumi 2018-m"
                    )

  x2 <- x_lang %>% px_values(values_df)

  expect_identical(px_values(x2), values_df)

  x3 <- x2 %>% px_values(NULL)

  expect_identical(px_values(x3), NULL)
})
