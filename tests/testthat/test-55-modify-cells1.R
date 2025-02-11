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
    dplyr::select(-precision) %>%
    dplyr::arrange(match(.data$`variable-code`, names(px_data(x))),
                   order
                   )

  x5 <- px_order(x, reverse_order)

  expect_identical(px_order(x5), reverse_order)

  x6 <- px_order(x, NULL)
  expect_identical(px_order(x6), NULL)
})


test_that('cells1 are properly created for multilingual file without CODES', {
  x <- px(get_px_file_path('multilingual_no_codes'))

  expect_identical(x$cells1,
                   tibble::tribble(
                     ~`variable-code`,  ~code, ~order, ~precision,
                               "type",    "a",     1L,   NA_real_,
                               "type",    "b",     2L,   NA_real_,
                               "type",    "c",     3L,   NA_real_,
                               "type",    "d",     4L,   NA_real_,
                             "gender",    "F",     1L,   NA_real_,
                             "gender",    "M",     2L,   NA_real_
                     )
                   )
})
