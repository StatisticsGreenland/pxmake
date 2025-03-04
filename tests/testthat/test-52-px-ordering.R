test_that("ordered factors are sorted", {
  tmp_px <- temp_px_file()

  age_levels <- c("17-24", "25-64", "7-16", "65+", "0-6")

  df <-
    population_gl %>%
    dplyr::mutate(age = factor(age, levels = age_levels,
                               ordered=TRUE
                               )
                  )

  px_ordering <-
    px(df)$cells1 %>%
    dplyr::filter(`variable-code` == "age") %>%
    dplyr::arrange(order) %>%
    dplyr::pull(code)


  expect_identical(age_levels, px_ordering)
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

test_that("px created from data frame orders variables properly", {
  df <-
    dplyr::tibble(sick = c(T, F, F, NA, T),
                  age = as.integer(c(1, 10, 100, NA, 42)),
                  income = c(9999, 20.1, 100, 999, NA),
                  name = c("E", "A", "B", "D", "C"),
                  gender = as.factor(c("male", "female", "male", NA, "female")),
                  education = factor(c("elementary", NA, "high school",
                                       "middle school", "high school"
                                       ),
                                     levels = c("high school", "middle school", "elementary"),
                                     ordered = TRUE
                                     ),
                  birth_date = as.Date(c("1966-07-30", NA, "1973-06-11",
                                         "1980-11-06", "1994-03-02"
                                         )
                                       ),
                  n = c(1, 20, 42, 49, 2)
                  )

  cells1_expect <- tibble::tribble(
  ~`variable-code`,           ~code, ~order, ~precision,
            "sick",         "FALSE",     1L,   NA_real_,
            "sick",          "TRUE",     2L,   NA_real_,
             "age",             "1",     1L,   NA_real_,
             "age",            "10",     2L,   NA_real_,
             "age",            "42",     3L,   NA_real_,
             "age",           "100",     4L,   NA_real_,
          "income",          "20.1",     1L,   NA_real_,
          "income",           "100",     2L,   NA_real_,
          "income",           "999",     3L,   NA_real_,
          "income",          "9999",     4L,   NA_real_,
            "name",             "A",     1L,   NA_real_,
            "name",             "B",     2L,   NA_real_,
            "name",             "C",     3L,   NA_real_,
            "name",             "D",     4L,   NA_real_,
            "name",             "E",     5L,   NA_real_,
          "gender",        "female",     1L,   NA_real_,
          "gender",          "male",     2L,   NA_real_,
       "education",   "high school",     1L,   NA_real_,
       "education", "middle school",     2L,   NA_real_,
       "education",    "elementary",     3L,   NA_real_,
      "birth_date",    "1966-07-30",     1L,   NA_real_,
      "birth_date",    "1973-06-11",     2L,   NA_real_,
      "birth_date",    "1980-11-06",     3L,   NA_real_,
      "birth_date",    "1994-03-02",     4L,   NA_real_
    ) %>%
    dplyr::mutate(order = as.numeric(order))

  expect_identical(px(df)$cells1, cells1_expect)
})
