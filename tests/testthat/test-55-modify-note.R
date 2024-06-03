test_that('NOTE(X) is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(px_note(x), NULL)
  expect_identical(px_notex(x),NULL)

  x2 <- px_note(x, value = "Note for entire table")
  expect_error(px_note(x, value = 4), regex = "wrong format")

  expect_identical(px_note(x2), "Note for entire table")
  expect_true(all(is.na(x2$variables2$note)))

  x_lang <- px_languages(x, c('da', 'kl'))

  expect_error(px_note(x_lang, value = c(4, 5)), regex = "wrong format")

  expect_identical(px_note(x_lang), NULL)

  dk_note_df <- dplyr::tibble(language = "da",
                              value = "En dansk note"
                              )

  x_lang2 <- px_note(x_lang, value = dk_note_df)
  expect_error(px_note(x_lang, value = data.frame(language = "da")),
                    regex = "wrong format"
               )

  expect_identical(px_note(x_lang2), dk_note_df)

  entire_table_note <- "Same note for da and kl"
  x_lang3 <- px_note(x_lang2, value = entire_table_note)

  same_note_df <- dplyr::tibble(language = c("da", "kl"),
                                value = entire_table_note
                                )

  expect_identical(px_note(x_lang3), same_note_df)

  expect_error(px_note(x, value = dplyr::tibble(language = "da",
                                             value = "A variable note",
                                             wrong_name = "invalid"
                                             )
                    ),
               regex = "invalid columns: wrong_name"
               )

  variable_note_df <- dplyr::tibble(`variable-code` = "gender",
                                    note = "A variable note"
                                    )

  x3 <- px_note(x, value = variable_note_df)

  expect_error(px_note(x, value = dplyr::tibble(`variable-code` = "gender",
                                             note = "A variable note",
                                             wrong_name = "invalid"
                                             )
                    ),
               regex = "invalid columns: wrong_name"
               )

  expect_identical(px_note(x3), variable_note_df)


  variable_note_lang_df <- dplyr::tibble(`variable-code` = "gender",
                                         language = c("da", "kl"),
                                         note = c("danish note",
                                                  "kalaallisut note"
                                                  )
                                         )

  x_lang4 <- px_note(x_lang, value = variable_note_lang_df)

  expect_identical(px_note(x_lang4), variable_note_lang_df)

  x_lang5 <-
    x_lang %>%
    px_note(value = entire_table_note) %>%
    px_note(value = variable_note_lang_df)

  expect_identical(px_note(x_lang5), list(same_note_df,
                                       variable_note_lang_df
                                       )
                   )

  x_lang6 <- px_note(x_lang, NULL)

  expect_identical(px_note(x_lang6), NULL)

  x_lang7 <- px_note(x_lang, value = list(variable_note_lang_df,
                                       same_note_df
                                       )
                  )

  expect_identical(px_note(x_lang7), list(same_note_df,
                                       variable_note_lang_df
                                       )
                   )

  expect_error(px_note(x_lang, value = list(c(1),
                                         variable_note_lang_df
                                         )
                    ),
               regex = "wrong format"
               )
})
