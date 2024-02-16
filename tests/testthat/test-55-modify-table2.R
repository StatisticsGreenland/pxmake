test_that('Table2 is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(last_updated(x), NULL)

  datetime <- '2020-01-01 10:00'

  x2 <- last_updated(x, datetime)
  expect_identical(last_updated(x2), '2020-01-01 10:00')


  x3 <- languages(x, c('en', 'dk'))
  expect_identical(last_updated(x3), NULL)

  x4 <- last_updated(x3, datetime)
  expect_identical(last_updated(x4), '2020-01-01 10:00')

  datetime_df <- dplyr::tibble(language = c('en', 'dk'),
                               value = c('2020-01-01 10:00',
                                         '2022-01-01 10:00'))

  x5 <- last_updated(x3, datetime_df)
  expect_identical(last_updated(x5), datetime_df)

  x6 <- last_updated(x5, NULL)
  expect_identical(last_updated(x6), NULL)

  expect_error(last_updated(x3, data.frame(language = c('sv', 'kl'),
                                           value = datetime)),
               regex = 'LANGUAGE'
               )
})
