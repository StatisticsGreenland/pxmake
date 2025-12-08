test_that("Data table can be returned sorted", {
  x1 <- px_from_table_name('BEXSTA')

  expect_error(px_data(x1, value = population_gl, sort = TRUE),
               regexp = "'sort' can only be used if 'value' is missing"
               )

  expect_error(px_data(x1, sort = "yes"),
               regexp = "must be TRUE or FALSE."
               )

  # Change order
  total_level_last <- dplyr::tribble(~`variable-code`, ~code, ~order,
                                     'gender',   'T',      4
                                     )

  x2 <-
    x1 %>%
    px_order(
      px_order(x1) %>%
        dplyr::anti_join(total_level_last,
                         by = dplyr::join_by(`variable-code`,
                                             code
                                             )
                         ) %>%
        dplyr::bind_rows(total_level_last)
    )

  expect_identical(px_data(x1), px_data(x2))
  expect_identical(px_data(x1, labels = TRUE), px_data(x2, labels = TRUE))

  expect_false(identical(px_data(x1, sort = TRUE),
                         px_data(x2, sort = TRUE)
                         )
               )

  expect_false(identical(px_data(x1, labels = TRUE, sort = TRUE),
                         px_data(x2, labels = TRUE, sort = TRUE)
                         )
               )

  x2_data_expected_codes <-
    px_data(x2) %>%
    dplyr::arrange(match(`place of birth`, c("T", "N", "S")),
                   match(gender, c("M", "K", "T"))
                   )

  x2_data_expected_labels <-
    px_data(x2, labels = TRUE) %>%
    dplyr::arrange(match(`place of birth`, c("Total", "Greenland", "Outside Greenland")),
                   match(gender, c("Men", "Women", "Total"))
                   )

  expect_identical(x2_data_expected_codes,
                   px_data(x2, sort = TRUE)
                   )

  expect_identical(x2_data_expected_labels,
                   px_data(x2, label = TRUE, sort = TRUE)
                   )
})



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

test_that('Elimination values and order is preserved ', {
  elimination_df <- dplyr::tibble(`variable-code` = 'gender',
                                  elimination = "all genders"
                                  )

  elimination_order <- dplyr::tibble(`variable-code` = 'gender',
                                     code = 'all genders',
                                     order = 0
                                     )
  x1 <-
    population_gl %>%
    px() %>%
    px_elimination(elimination_df) %>%
    px_order(elimination_order)

  population_gl_2024 <-
    population_gl %>%
    dplyr::filter(year == 2024)

  x2 <-
    x1 %>%
    px_data(population_gl_2024)

  expect_identical(px_elimination(x2), elimination_df)

  px_order(x2) %>%
    dplyr::semi_join(elimination_df,
                     by = c("variable-code" = "variable-code",
                            "code" = "elimination"
                            )
                     ) %>%
    expect_identical(elimination_order)
})

test_that("Labels can be returned", {
  x1 <- px_from_table_name('BEXSTA')

  expect_error(px_data(x1, value = population_gl, labels = TRUE),
               regexp = "can only be used"
               )

  expect_error(px_data(x1, labels = 'fr'),
               regexp = "Language.*is not defined"
               )

  expect_identical(px_data(x1, labels = TRUE),
                   px_data(x1, labels = 'en')
                   )

  expect_identical(head(px_data(x1, labels = 'kl'), 3),
                   tibble::tribble(
                     ~`place of birth`,  ~gender,  ~time, ~persons,
                     "Kalaallit Nunaat", "Arnat", "2018",    24392,
                     "Kalaallit Nunaat", "Arnat", "2019",    24434,
                     "Kalaallit Nunaat", "Arnat", "2020",    24452
                     )
                   )

  x2 <- px(population_gl)

  expect_identical(px_data(x2, labels = TRUE),
                   px_data(x2)
                   )
})


