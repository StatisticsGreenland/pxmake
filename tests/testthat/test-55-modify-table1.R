test_that('Table1 is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px() %>%
    charset("ANSI") %>%
    creation_date("2019-01-01 10:00") %>%
    matrix("BEXSTA") %>%
    decimals("1")

  expect_equal(charset(x), "ANSI")
  expect_equal(creation_date(x), "2019-01-01 10:00")
  expect_equal(matrix(x), "BEXSTA")
  expect_equal(decimals(x), "1")
})
