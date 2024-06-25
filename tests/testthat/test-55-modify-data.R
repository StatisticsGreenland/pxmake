test_that('data is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%d
    px()

  expect_identical(x$data, px_data(x))

  mini_data <- x$data[1:5, ]

  x2 <- px_data(x, mini_data)

  expect_identical(mini_data, px_data(x2))
})
