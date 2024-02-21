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

  base_px5$table1 <- data.frame(keyword = "LAST-UPDATED",
                                value   = "2020-01-01 10:00"
                                )

  expect_error(validate_px(base_px5),
               regexp = "'table1' contains misplaced keywords"
               )

  base_px6 <- get_base_px()
  base_px6$table2 <-
    data.frame(keyword = "NEXT-UPDATE",
               value   = "2020-01-01 10:00"
               ) %>%
    align_data_frames(get_base_table2())

  expect_error(validate_px(base_px6),
               regexp = "'table2' contains misplaced keywords"
               )

  base_px7 <- get_base_px()

  base_px7$languages <-
    data.frame(language = "en") %>%
    align_data_frames(get_base_languages())

  base_px7$table2 <-
    dplyr::bind_rows(base_px7$table2,
                     data.frame(keyword = "TITLE", language = "dk")
                     )

  expect_error(validate_px(base_px7),
               regexp = "languages that are not defined"
               )

  bexsta <- px(get_data_path('BEXSTA'))

  bexsta1 <- bexsta
  bexsta1$variables1 <- dplyr::bind_rows(bexsta1$variables1,
                                         dplyr::tibble(`variable-code` = "fisk")
                                         )

  expect_error(validate_px(bexsta1), regexp = "variables1.*not in x\\$data:\\s*fisk")

  bexsta2 <- bexsta
  bexsta2$variables2 <- dplyr::bind_rows(bexsta2$variables2,
                                         dplyr::tibble(`variable-code` = "ost",
                                                       `variable-label` = "Ost")
                                         )

  expect_error(validate_px(bexsta2), regexp = c("variables2.*not in x\\$data:\\s*ost"))

  bexsta3 <- bexsta
  bexsta3$codelists1 <- dplyr::bind_rows(bexsta3$codelists1,
                                         dplyr::tibble(`variable-code` = "sovs",
                                                       code = "1"
                                                       )
                                         )

  expect_error(validate_px(bexsta3), regexp = "codelists1.*not in x\\$data:\\s*sovs")

  bexsta4 <- bexsta
  bexsta4$codelists2 <- dplyr::bind_rows(bexsta4$codelists2,
                                         dplyr::tibble(`variable-code` = "ice",
                                                       code = "1"
                                                       )
                                         )

  expect_error(validate_px(bexsta4), regexp = "codelists2.*not in x\\$data:\\s*ice")
})
