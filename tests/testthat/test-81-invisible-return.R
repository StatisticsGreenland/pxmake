test_that("pxmake returns px object invisibly", {
  px <- pxmake(get_metadata_path('FOTEST'))

  validate_px(px)

  expect_true(TRUE)
})

test_that("metamake returns px object invisibly", {
  px <- metamake(get_px_file_path('TUX01'))

  validate_px(px)

  expect_true(TRUE)
})
