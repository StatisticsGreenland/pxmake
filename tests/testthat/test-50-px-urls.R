github_raw_url <- "https://raw.githubusercontent.com/StatisticsGreenland/pxmake/"

test_that("px runs without errors (URL)", {
  skip_if_offline(host = "raw.githubusercontent.com")

  px(input = paste0(github_raw_url, "main/tests/testthat/fixtures/px/TUX01.px"))

  expect_true(TRUE)
})

test_that("px can take a parquet url is a input", {
  skip_if_offline(host = "raw.githubusercontent.com")

  px(input = paste0(github_raw_url, "main/tests/testthat/fixtures/micro.parquet"))

  expect_true(TRUE)
})

