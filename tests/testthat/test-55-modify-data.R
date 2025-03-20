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
  population_gl_new <-
    population_gl %>%
    dplyr::bind_rows(dplyr::tibble(gender='total',
                                   age='0-6',
                                   year = '2004',
                                   n = 1
                                   )
                     ) %>%
    dplyr::mutate(year = stringr::str_sub(year, 3, 4)) %>%
    dplyr::rename(shortyear = year)

  x1 <-
    population_gl %>%
    px()

  x2 <-
    x1 %>%
    px_data(population_gl_new)

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
})
