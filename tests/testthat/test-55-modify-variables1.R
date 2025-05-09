test_that('Variables is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_equal(px_stub(x), c("place of birth", "gender"))

  # Change time to stub variable
  x2 <- px_stub(x, value = "time")
  expect_equal(px_stub(x2), c("time", "place of birth", "gender"))
  expect_equal(names(x2$variables1), names(get_base_variables1()))

  # Change order
  new_order <- c("gender", "time", "place of birth")
  x3 <- px_stub(x, value = new_order)
  expect_equal(px_stub(x3), new_order)

  x4 <- px_heading(x, value = new_order)
  expect_equal(px_heading(x4), new_order)
  expect_equal(px_stub(x4), as.character())

  x5 <- px_figures(x, value = "time")

  expect_equal(px_figures(x5), "time")
  expect_equal(px_heading(x5), as.character())
  expect_equal(px_stub(x5), c("persons", "place of birth", "gender"))

  expect_equal(px_timeval(x), NULL)

  x6 <- px_heading(x5, c("gender"))

  x6_stub_vars <- px_stub(x6)

  expect_identical(dplyr::filter(x5$variables1, `variable-code` %in% x6_stub_vars),
                   dplyr::filter(x6$variables1, `variable-code` %in% x6_stub_vars)
                   )

  x7 <- px_timeval(x, value = "time")
  expect_equal(px_timeval(x7), "time")

  x8 <- px_timeval(x7, value = "gender")
  expect_equal(px_timeval(x8), "gender")

  x9 <- px_timeval(x8, NULL)
  expect_equal(px_timeval(x9), NULL)

  elimination_df <- dplyr::tribble(~`variable-code`, ~elimination,
                                   "gender", "Total"
                                   )

  x10 <- px_elimination(x, elimination_df)
  expect_equal(px_elimination(x10), elimination_df)

  x11 <- px_elimination(x10, 'T')

  expect_equal(px_elimination(x11), dplyr::tribble(~`variable-code`, ~elimination,
                                                   "place of birth", "T",
                                                   "gender", "T",
                                                   "time", "T"
                                                   )
               )

  x11 <- px_elimination(x10, NULL)

  expect_equal(px_elimination(x11), NULL)
})

test_that("VARIABLE-TYPE is changed", {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_equal(px_variable_type(x), NULL)

  variable_type_df1 <- dplyr::tibble(`variable-code` = "gender",
                                     `variable-type` = "Time"
                                     )

  x1 <- px_variable_type(x, variable_type_df1)

  expect_equal(px_variable_type(x1), variable_type_df1)

  x2 <- px_variable_type(x, NULL)

  expect_equal(px_variable_type(x2), NULL)

  variable_type_df2_empty <- dplyr::filter(variable_type_df1, FALSE)

  x3 <- px_variable_type(x1, variable_type_df2_empty)

  expect_equal(px_variable_type(x3), variable_type_df1)
})

test_that("CONTVARIABLE is changed", {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  x_lang <- px_languages(x, c("kl", "da"))

  expect_equal(px_contvariable(x), NULL)

  x1 <- px_contvariable(x, "gender")

  expect_equal(px_contvariable(x1), "gender")

  # Contvariable indexes some variables in table2
  expect_equal(px_units(x1), dplyr::tibble(code = c(NA, "K", "M", "T"),
                                           language = NA_character_,
                                           value = default_value("UNITS")
                                           )
               )

  x1_lang <- px_contvariable(x_lang, "gender")

  expect_equal(px_units(x1_lang), dplyr::tibble(`code` = c(rep(NA, 2),
                                                           rep("K", 2),
                                                           rep("M", 2),
                                                           rep("T", 2)
                                                           ),
                                                language = rep(c("kl", "da"), 4),
                                                value = default_value("UNITS")
                                                )
               )

  x2 <- px_contvariable(x1, NULL)

  expect_equal(px_contvariable(x2), NULL)

  # Setting CONTVARIABLE to NULL removes indexing in table2
  expect_equal(px_units(x2), "units")
})

test_that('stub and heading modifies acrosscells', {
  cellnote_df1 <- dplyr::tibble(`place of birth` = "*",
                                gender = "K",
                                time = "2018",
                                cellnote = "This is a cellnote"
                                )

  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px() %>%
    px_cellnote(cellnote_df1)

  x1 <-
    x %>%
    px_stub('time')

  expect1 <-
    x$acrosscells %>%
    dplyr::relocate(time)

  expect_identical(expect1, x1$acrosscells)

  x2 <-
    x1 %>%
    px_heading(c('gender', 'place of birth'))

  expect2 <-
    x$acrosscells %>%
    dplyr::relocate(time, gender, `place of birth`)

  expect_identical(expect2, x2$acrosscells)
})

test_that('changing px_figures removes it from cells', {
  x <-
    population_gl %>%
    dplyr::relocate(n) %>%
    px() %>%
    px_figures("n")

  x$cells1 %>%
    dplyr::filter(`variable-code` == "n") %>%
    nrow() %>%
    expect_equal(0)


  x$cells2 %>%
    dplyr::filter(`variable-code` == "n") %>%
    nrow() %>%
    expect_equal(0)
})
