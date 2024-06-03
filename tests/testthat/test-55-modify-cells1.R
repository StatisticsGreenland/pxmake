test_that('cells1 is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(px_precision(x), NULL)

  precision_df1 <- dplyr::tibble(`variable-code` = 'gender',
                                 code = 'K',
                                 precision = 1
                                 )

  x2 <- px_precision(x, precision_df1)
  expect_identical(px_precision(x2), precision_df1)

  precision_df2 <- dplyr::tibble(`variable-code` = 'gender',
                                 precision = 2
                                 )

  x3 <- px_precision(x, precision_df2)

  precision_df2_expect <- tidyr::crossing(precision_df2,
                                          code = c('K', 'M', 'T')
                                          ) %>%
    dplyr::relocate(precision, .after = last_col())

  expect_identical(px_precision(x3), precision_df2_expect)

  x4 <- px_precision(x, NULL)
  expect_identical(px_precision(x4), NULL)


  reverse_order <-
    px_precision(x, data.frame(precision = 1)) %>%
    px_precision() %>%
    dplyr::mutate(order = rev(dplyr::row_number())) %>%
    dplyr::select(-precision)

  x5 <- order(x, reverse_order)

  expect_identical(order(x5), reverse_order)

  x6 <- order(x, NULL)
  expect_identical(order(x6), NULL)
})
