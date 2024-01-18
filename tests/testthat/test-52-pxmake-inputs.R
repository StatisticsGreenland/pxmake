test_that("pxmake accepts a data frame object", {
  table_name <- "BEXSTA"

  px1   <- temp_px_file()
  meta1 <- get_metadata_path(table_name)
  data1 <- get_data_path(table_name)
  px2   <- temp_px_file()
  df    <- readRDS(get_data_path(table_name))

  pxmake_clean(meta1, px1, data = data1)
  pxmake_clean(meta1, px2, data = df)

  expect_equal_lines(px1, px2)
})
