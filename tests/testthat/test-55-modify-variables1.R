test_that('Variables is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_equal(stub(x), c("place of birth", "gender"))

  # Change time to stub variable
  p2 <- stub(x, variables = "time")
  expect_equal(stub(p2), c("time", "place of birth", "gender"))

  # Change order
  new_order <- c("gender", "time", "place of birth")
  p3 <- stub(x, variables = new_order)
  expect_equal(stub(p3), new_order)

  p4 <- heading(x, variables = new_order)
  expect_equal(heading(p4), new_order)
  expect_equal(stub(p4), as.character())

  p5 <- figures(x, variable = "time")

  expect_equal(figures(p5), "time")
  expect_equal(heading(p5), as.character())
  expect_equal(stub(p5), c("persons", "gender", "place of birth"))

  expect_equal(timeval(x), character(0))

  p6 <- timeval(x, variable = "time")
  expect_equal(timeval(p6), "time")

  p7 <- timeval(p6, variable = "gender")
  expect_equal(timeval(p7), "gender")
})
