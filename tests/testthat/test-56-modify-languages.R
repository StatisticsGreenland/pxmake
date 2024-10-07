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

  # Creates mandatory keywords for all languages
  expect_all_mandatory_keywords_return_non_na_values <- function(x) {
    mandatory_keyword_values <-
      mandatory_keywords() %>%
      sapply(function(keyword) {
        f <- get(keyword_to_function(keyword))
        f(x)
      })

    any_na_values <- function(l) {
      if (is.data.frame(l)) {
        any(is.na(l$value))
      } else {
        is.na(l)
      }
    }

    sapply(mandatory_keyword_values, any_na_values) %>%
      any() %>%
      expect_false()
  }

  x6 <-
    x %>%
    px_language('da')

  x7 <-
    x %>%
    px_languages(c("da", "kl"))

  expect_all_mandatory_keywords_return_non_na_values(x6)
  expect_all_mandatory_keywords_return_non_na_values(x7)

  # Runs without errors
  x %>%
    px_languages(c("en", "kl")) %>%
    px_language("en")

  x %>%
    px_languages(c("en", "kl")) %>%
    px_language("dk")

  # Setting language doesn't interfere with languages
  x_before <-
    population_gl %>%
    px() %>%
    px_languages(c("en", "da", "kl"))

  x_after <-
    x_before %>%
    px_language("en")

  expect_equal(px_languages(x_after), c("en", "da", "kl"))

  expect_identical(x_before %>% purrr::pluck("table2"),
                   x_after  %>% purrr::pluck("table2")
                   )
})
