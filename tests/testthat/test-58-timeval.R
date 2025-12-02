test_that("short TIMEVAl syntax is equivalent to long", {
  x1 <- px(get_px_file_path("TIMEVAL_long"))
  x2 <- px(get_px_file_path("TIMEVAL_short"))

  expect_identical(x1, x2)
})
