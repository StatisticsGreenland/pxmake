test_that('Table1 keywords are modified and removed', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px() %>%
    charset("ANSI") %>%
    creation_date("2019-01-01 10:00") %>%
    matrix("BEXSTA") %>%
    decimals("1") %>%
    last_updated("2020-01-01 10:00") %>%
    next_update(format("2022-01-01 10:00", format='%Y%m%d %H:%M')) %>%
    subject_code('BEXSTA')

  expect_identical(charset(x), "ANSI")
  expect_identical(creation_date(x), "2019-01-01 10:00")
  expect_identical(matrix(x), "BEXSTA")
  expect_identical(decimals(x), "1")
  expect_identical(last_updated(x), "2020-01-01 10:00")
  expect_identical(next_update(x), "2022-01-01 10:00")
  expect_identical(subject_code(x), "BEXSTA")

  x2 <-
    x %>%
    charset(NULL) %>%
    creation_date(NULL) %>%
    last_updated(NULL) %>%
    next_update(NULL)

  expect_identical(charset(x2), NULL)
  expect_identical(creation_date(x2), NULL)
  expect_identical(last_updated(x2), NULL)
  expect_identical(next_update(x2), NULL)

  expect_error(matrix(x, NULL), regexp = "mandatory")
  expect_error(decimals(x, NULL), regexp = "mandatory")
  expect_error(subject_code(x, NULL), regexp = "mandatory")
})
