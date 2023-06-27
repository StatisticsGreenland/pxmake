test_that("metamake can create rds file", {
  table_name   <- 'BEXSTA'
  px1     <- temp_px_file()
  meta1   <- get_metadata_path(table_name)
  data1   <- get_data_path(table_name)

  rds_out <- temp_rds_file()
  meta2   <- temp_xlsx_file()
  px2     <- temp_px_file()

  pxmake_clean(meta1, px1, data1)
  metamake_clean(px1, meta2, data_path = rds_out)
  pxmake_clean(meta2, px2, rds_out)

  expect_true(file.exists(rds_out))
  expect_equal_lines(px1, px2)
})
