test_that('variables 2 is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(px_domain(x), NULL)
  expect_identical(px_elimination(x), NULL)

  domain_str <- "aggregation list"

  x2 <- px_domain(x, domain_str)
  expect_identical(px_domain(x2), domain_str)

  x3 <- px_languages(x, c('en', 'da'))
  expect_identical(px_domain(x3), NULL)

  x4 <- px_domain(x3, domain_str)
  expect_identical(px_domain(x4), domain_str)

  domain_df <- dplyr::tibble(`variable-code` = "time",
                             language = c("en", "da"),
                             domain = c("aggregation list1",
                                       "aggregation list2"
                                       )
                             )

  x5 <- px_domain(x3, domain_df)
  expect_identical(px_domain(x5), domain_df)

  domain_df2 <- dplyr::tibble(`variable-code` = "gender",
                              language = c("en"),
                              domain = c("aggregation list1")
                              )

  x6 <- px_domain(x3, domain_df2)
  expect_identical(px_domain(x6), domain_df2)

  x7 <- px_domain(x5, NULL)
  expect_identical(px_domain(x7), NULL)
})
