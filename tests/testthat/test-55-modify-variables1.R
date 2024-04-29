test_that('Variables is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_equal(stub(x), c("place of birth", "gender"))

  # Change time to stub variable
  x2 <- stub(x, variables = "time")
  expect_equal(stub(x2), c("time", "place of birth", "gender"))
  expect_equal(names(x2$variables1), names(get_base_variables1()))

  # Change order
  new_order <- c("gender", "time", "place of birth")
  x3 <- stub(x, variables = new_order)
  expect_equal(stub(x3), new_order)

  x4 <- heading(x, variables = new_order)
  expect_equal(heading(x4), new_order)
  expect_equal(stub(x4), as.character())

  x5 <- figures(x, variable = "time")

  expect_equal(figures(x5), "time")
  expect_equal(heading(x5), as.character())
  expect_equal(stub(x5), c("persons", "place of birth", "gender"))

  expect_equal(timeval(x), character(0))

  x6 <- heading(x5, c("gender"))

  x6_stub_vars <- stub(x6)

  expect_identical(dplyr::filter(x5$variables1, `variable-code` %in% x6_stub_vars),
                   dplyr::filter(x6$variables1, `variable-code` %in% x6_stub_vars)
                   )

  x7 <- timeval(x, variable = "time")
  expect_equal(timeval(x7), "time")

  x8 <- timeval(x7, variable = "gender")
  expect_equal(timeval(x8), "gender")
})

test_that("VARIABLE-TYPE is changed", {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_equal(variable_type(x), NULL)

  variable_type_df1 <- dplyr::tibble(`variable-code` = "time",
                                     type = "Time"
                                     )

  x1 <- variable_type(x, variable_type_df1)

  expect_equal(variable_type(x1), variable_type_df1)

  x2 <- variable_type(x, NULL)

  expect_equal(variable_type(x2), NULL)

  variable_type_df2_empty <- dplyr::filter(variable_type_df1, FALSE)

  x3 <- variable_type(x1, variable_type_df2_empty)

  expect_equal(variable_type(x3), variable_type_df1)
})

test_that('stub and heading modifies acrosscell', {
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
    cellnote(cellnote_df1)

  x1 <-
    x %>%
    stub('time')

  expect1 <-
    x$acrosscell %>%
    dplyr::relocate(time)

  expect_identical(expect1, x1$acrosscell)

  x2 <-
    x1 %>%
    heading(c('gender', 'place of birth'))

  expect2 <-
    x$acrosscell %>%
    dplyr::relocate(time, gender, `place of birth`)

  expect_identical(expect2, x2$acrosscell)
})
