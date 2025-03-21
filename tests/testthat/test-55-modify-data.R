test_that('data is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(x$data, px_data(x))

  mini_data <- x$data[1:5, ]

  x2 <- px_data(x, mini_data)

  expect_identical(mini_data, px_data(x2))

  x3 <- px_data(x, NULL)

  expect_identical(dplyr::filter(x$data, FALSE), px_data(x3))
})

test_that('modifying data updates metadata', {
  # Use a new data set where one variable changes name and one changes levels.
  population_gl_new <-
    population_gl %>%
    dplyr::mutate(age = dplyr::if_else(age %in% c('0-6', '7-16'),
                                       '0-16',
                                       age
                                       )
                   ) %>%
    dplyr::group_by(gender, age, year) %>%
    dplyr::summarise(n = sum(n), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = stringr::str_sub(year, 3, 4)) %>%
    dplyr::rename(shortyear = year)

  x1 <-
    population_gl %>%
    px()

  x2 <-
    x1 %>%
    px_data(population_gl_new)


  ## Variables
  old_names <- names(population_gl)
  new_names <- names(population_gl_new)
  new_names_stub_heading <- setdiff(new_names, 'n')
  removed_vars <- setdiff(old_names, new_names)

  expect_identical(x1$languages,  x2$languages)
  expect_identical(x1$table1,     x2$table1)
  expect_identical(x1$table2,     x2$table2)

  expect_true(all(new_names %in% x2$variables1$`variable-code`))
  expect_true(all(new_names %in% x2$variables2$`variable-code`))
  expect_true(all(new_names_stub_heading %in% x2$cells1$`variable-code`))
  expect_true(all(new_names_stub_heading %in% x2$cells2$`variable-code`))

  expect_false(any(removed_vars %in% x2$variables1$`variable-code`))
  expect_false(any(removed_vars %in% x2$variables2$`variable-code`))
  expect_false(any(removed_vars %in% x2$cells1$`variable-code`))
  expect_false(any(removed_vars %in% x2$cells2$`variable-code`))

  expect_identical(x2$acrosscell,
                   get_base_acrosscells(c(px_stub(x2), px_heading(x2)))
                   )

  ## Codes
  data_set_codes <- function(df) {
    df %>%
      dplyr::select(-n) %>%
      tidyr::pivot_longer(everything(),
                          names_to = "variable-code",
                          values_to = 'code'
                          ) %>%
      dplyr::distinct_all()
  }

  old_codes <- data_set_codes(population_gl)

  df_codes <- data_set_codes(population_gl_new)

  removed_codes <-
    old_codes  %>%
    dplyr::anti_join(df_codes, by = dplyr::join_by(`variable-code`, code))

  new_codes <-
    df_codes %>%
    dplyr::anti_join(old_codes, by = dplyr::join_by(`variable-code`, code))

  dplyr::semi_join(removed_codes, x2$cells1,
                   by = dplyr::join_by(`variable-code`, code)
                   ) %>%
    nrow() %>%
    expect_equal(0)

  expect_no_matches <- function(df1, df2) {
    dplyr::semi_join(df1, df2,
                     by = dplyr::join_by(`variable-code`, code)
                     ) %>%
      nrow() %>%
      expect_equal(0)
  }

  expect_no_matches(removed_codes, x2$cells1)
  expect_no_matches(removed_codes, x2$cells2)

  expect_all_matches <- function(df1, df2) {
    dplyr::anti_join(df1, df2,
                     by = dplyr::join_by(`variable-code`, code)
                     ) %>%
      nrow() %>%
      expect_equal(0)
  }

  expect_all_matches(new_codes, x2$cells1)
  expect_all_matches(new_codes, x2$cells2)

  ## Multilingual
  x_lang1 <-
    population_gl %>%
    px() %>%
    px_languages(c('en', 'fr'))

  x_lang2 <-
    x_lang1 %>%
    px_data(population_gl_new)

  x_lang2$variables2 %>%
    dplyr::filter(`variable-code` == "shortyear") %>%
    dplyr::pull(language) %>%
    expect_identical(c('en', 'fr'))

  expect_identical(nrow(x_lang2$cells2), 18L)
  expect_identical(x_lang2$cells2$language, rep(c('en', 'fr'), 9))
})


