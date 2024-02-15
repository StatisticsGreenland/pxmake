test_that('LANGUAGE modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_true(is.null(language(x)))
  expect_identical(languages(x), NULL)

  x2 <- language(x, "sv")
  expect_identical(language(x2), "sv")
  expect_identical(languages(x2), NULL)

  x3 <- languages(x2, "sv")
  expect_identical(language(x3), "sv")
  expect_identical(languages(x3), c("sv"))

  expect_error(languages(x3, "en"), regexp = "LANGUAGE is not in")

  x4 <-
    x3 %>%
    language(NULL) %>%
    languages("en")

  expect_identical(language(x4), NULL)
  expect_identical(languages(x4), c("en"))

  x5 <- languages(x4, NULL)

  expect_identical(languages(x5), NULL)

  # Runs without errors
  x %>%
    languages(c("en", "kl")) %>%
    language("en")

  x %>%
    languages(c("en", "kl")) %>%
    language("dk")
})
