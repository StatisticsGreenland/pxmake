test_that('LANGUAGE modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_true(is.null(px_language(x)))
  expect_identical(px_languages(x), NULL)

  x2 <- px_language(x, "sv")
  expect_identical(px_language(x2), "sv")
  expect_identical(px_languages(x2), NULL)

  x3 <- px_languages(x2, "sv")
  expect_identical(px_language(x3), "sv")
  expect_identical(px_languages(x3), c("sv"))

  expect_error(px_languages(x3, "en"), regexp = "LANGUAGE is not in")

  x4 <-
    x3 %>%
    px_language(NULL) %>%
    px_languages("en")

  expect_identical(px_language(x4), NULL)
  expect_identical(px_languages(x4), c("en"))

  x5 <- px_languages(x4, NULL)

  expect_identical(px_languages(x5), NULL)

  # Runs without errors
  x %>%
    px_languages(c("en", "kl")) %>%
    px_language("en")

  x %>%
    px_languages(c("en", "kl")) %>%
    px_language("dk")
})
