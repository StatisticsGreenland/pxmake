test_that('Variables is modified', {
  p <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_equal(stub_variables(p), c("place of birth", "gender"))

  # Change time to stub variable
  p2 <- stub(p, variables = "time")
  expect_equal(stub_variables(p2), c("time", "place of birth", "gender"))

  # Change order
  new_order <- c("gender", "time", "place of birth")
  p3 <- stub(p, variables = new_order)
  expect_equal(stub_variables(p3), new_order)

  p4 <- heading(p, variables = new_order)
  expect_equal(heading_variables(p4), new_order)
  expect_equal(stub_variables(p4), as.character())

  p5 <- figures(p, variable = "time")

  expect_equal(figures_variable(p5), "time")
  expect_equal(heading_variables(p5), as.character())
  expect_equal(stub_variables(p5), c("persons", "gender", "place of birth"))

  expect_equal(time_variable(p), character(0))

  p6 <- timeval(p, variable = "time")
  expect_equal(time_variable(p6), "time")

  p7 <- timeval(p6, variable = "gender")
  expect_equal(time_variable(p7), "gender")
})
