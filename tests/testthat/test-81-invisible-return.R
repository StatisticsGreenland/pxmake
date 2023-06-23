test_that("pxmake returns rds object invisibly", {
  rds <- pxmake(get_metadata_path('FOTEST'))

  expect_true(is_rds_list(rds))

  pxmake_clean(rds, temp_px_file())
})

test_that("metamake returns rds object invisibly", {
  rds <- metamake(get_px_file_path('TUX01'))

  metamake_clean(rds, temp_xlsx_file())

  expect_true(is_rds_list(rds))
})
