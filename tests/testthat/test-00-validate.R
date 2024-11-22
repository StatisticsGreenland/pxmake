test_that("error - invalid px object", {
  expect_error(px_validate(1),
               regexp = "px object must be a list"
               )

  base_px <- get_base_px()
  base_px$cells1 <- NULL

  expect_error(px_validate(base_px),
               regexp = "missing these names:"
               )

  base_px2 <- get_base_px()
  base_px2$fisk <- data.frame()

  expect_error(px_validate(base_px2),
               regexp = "invalid names:"
               )

  base_px3 <- get_base_px()
  base_px3$languages <- c("en")

  expect_error(px_validate(base_px3),
               regexp = "list of data frames"
               )

  base_px4 <- get_base_px()
  base_px4$table1 <- data.frame(Keyword = as.character())

  expect_error(px_validate(base_px4),
               regexp = "px object: 'table1' is missing these columns: keyword"
               )

  base_px5 <- get_base_px()

  base_px5$table1 <- data.frame(keyword = "LAST-UPDATED",
                                value   = "2020-01-01 10:00"
                                )

  expect_error(px_validate(base_px5),
               regexp = "'table1' contains misplaced keywords"
               )

  base_px6 <- get_base_px()
  base_px6$table2 <-
    data.frame(keyword = "NEXT-UPDATE",
               value   = "2020-01-01 10:00"
               ) %>%
    align_data_frames(get_base_table2())

  expect_error(px_validate(base_px6),
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

  expect_error(px_validate(base_px7),
               regexp = "languages that are not defined"
               )

  bexsta <- px(get_data_path('BEXSTA'))

  bexsta1 <- bexsta
  bexsta1$variables1 <- dplyr::bind_rows(bexsta1$variables1,
                                         dplyr::tibble(`variable-code` = "fisk")
                                         )

  expect_error(px_validate(bexsta1), regexp = "variables1.*not in x\\$data:\\s*fisk")

  bexsta2 <- bexsta
  bexsta2$variables2 <- dplyr::bind_rows(bexsta2$variables2,
                                         dplyr::tibble(`variable-code` = "ost",
                                                       `variable-label` = "Ost")
                                         )

  expect_error(px_validate(bexsta2), regexp = c("variables2.*not in x\\$data:\\s*ost"))

  bexsta3 <- bexsta
  bexsta3$cells1 <- dplyr::bind_rows(bexsta3$cells1,
                                         dplyr::tibble(`variable-code` = "sovs",
                                                       code = "1"
                                                       )
                                         )

  expect_error(px_validate(bexsta3), regexp = "cells1.*not in x\\$data:\\s*sovs")

  bexsta4 <- bexsta
  bexsta4$cells2 <- dplyr::bind_rows(bexsta4$cells2,
                                         dplyr::tibble(`variable-code` = "ice",
                                                       code = "1"
                                                       )
                                         )

  expect_error(px_validate(bexsta4), regexp = "cells2.*not in x\\$data:\\s*ice")

  bexsta5 <- bexsta
  bexsta5$data$new_variable <- 1

  expect_error(px_validate(bexsta5), regexp = "not defined in x\\$variables1:\\s*new_variable")

  bexsta5$variables1 <- dplyr::bind_rows(bexsta5$variables1,
                                         dplyr::tibble(`variable-code` = "new_variable",
                                                       pivot = "STUB")
                                         )

  expect_error(px_validate(bexsta5), regexp = "not defined in x\\$variables2:\\s*new_variable")

  bexsta5$variables2 <- dplyr::bind_rows(bexsta5$variables2,
                                         dplyr::tibble(`variable-code` = "new_variable",
                                                       `variable-label` = "New variable"
                                                       )
                                         )

  expect_identical(px_validate(bexsta5), bexsta5)
})
