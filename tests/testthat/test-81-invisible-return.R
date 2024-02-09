test_that("pxmake returns px object invisibly", {
  x <- pxmake(get_metadata_path('FOTEST'))

  validate_px(x)

  expect_true(TRUE)
})

test_that("metamake returns px object invisibly", {
  x <- metamake(get_px_file_path('TUX01'))

  validate_px(x)

  expect_true(TRUE)
})
