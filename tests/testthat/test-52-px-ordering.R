test_that("ordered factors are sorted", {
  tmp_px <- temp_px_file()

  age_levels <- c("17-24", "25-64", "7-16", "65+", "0-6")

  df <-
    population_gl %>%
    dplyr::mutate(age = factor(age, levels = age_levels,
                               ordered=TRUE
                               )
                  )

  px_df <-
    df %>%
    px() %>%
    px_data()

  expect_identical(age_levels, levels(px_df$age))
})

test_that("numerics are sorted", {
  tmp_px <- temp_px_file()

  heights <- c(1, 2, 10, 20, 100, 200)

  population_gl %>%
    dplyr::mutate(height = rep(heights, 5)) %>%
    dplyr::relocate(n, .after = dplyr::last_col()) %>%
    px() %>%
    px_save(tmp_px)

  height_order_in_px <-
    px(tmp_px)$cells1 %>%
    dplyr::filter(`variable-code` == "height") %>%
    dplyr::pull(code)

  expect_identical(as.character(heights), height_order_in_px)
})
