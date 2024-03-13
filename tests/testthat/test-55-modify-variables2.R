test_that('variables 2 is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(domain(x), NULL)
  expect_identical(elimination(x), NULL)

  domain_str <- "aggregation list"

  x2 <- domain(x, domain_str)
  expect_identical(domain(x2), domain_str)

  x3 <- languages(x, c('en', 'da'))
  expect_identical(domain(x3), NULL)

  x4 <- domain(x3, domain_str)
  expect_identical(domain(x4), domain_str)

  domain_df <- dplyr::tibble(`variable-code` = "time",
                             language = c("en", "da"),
                             domain = c("aggregation list1",
                                       "aggregation list2"
                                       )
                             )

  x5 <- domain(x3, domain_df)
  expect_identical(domain(x5), domain_df)

  domain_df2 <- dplyr::tibble(`variable-code` = "gender",
                              language = c("en"),
                              domain = c("aggregation list1")
                              )

  x6 <- domain(x3, domain_df2)
  expect_identical(domain(x6), domain_df2)

  x7 <- domain(x5, NULL)
  expect_identical(domain(x7), NULL)
})
