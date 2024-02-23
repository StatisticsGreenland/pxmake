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
