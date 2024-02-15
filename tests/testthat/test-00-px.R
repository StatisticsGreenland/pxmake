test_that("error - invalid px object", {
  expect_error(validate_px(1),
               regexp = "px object must be a list"
               )

  base_px <- get_base_px()
  base_px$codelists1 <- NULL

  expect_error(validate_px(base_px),
               regexp = "px object is missing these names:"
               )

  base_px2 <- get_base_px()
  base_px2$fisk <- data.frame()

  expect_error(validate_px(base_px2),
               regexp = "px object contains invalid names:"
               )

  base_px3 <- get_base_px()
  base_px3$languages <- c("en")

  expect_error(validate_px(base_px3),
               regexp = "px object element 'languages' must be a data frame"
               )

  base_px4 <- get_base_px()
  base_px4$table1 <- data.frame(Keyword = as.character())

  expect_error(validate_px(base_px4),
               regexp = "px object is missing these names in element 'table1':"
               )

  base_px5 <- get_base_px()

  base_px5$languages <-
    data.frame(language = "en") %>%
    align_data_frames(get_base_languages())

  base_px5$table2 <-
    dplyr::bind_rows(base_px5$table2,
                     data.frame(keyword = "TITLE", language = "dk")
                     )

  expect_error(validate_px(base_px5),
               regexp = "languages that are not defined"
               )
})

test_that("valid px object", {
  expect_valid_px <- function(x) {
    expect_equal(x, validate_px(x))
  }

  expect_valid_px(get_base_px())
})
