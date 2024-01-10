test_that("pxmake can take 3 types of input and give the same result", {
  px1 <- temp_px_file()
  px2 <- temp_px_file()
  px3 <- temp_px_file()

  table_name <- "BEXSTA"
  xlsx       <- get_metadata_path(table_name)
  data_path  <- get_data_path(table_name)

  # option 1 (see ?pxmake)
  pxmake_clean(xlsx, px1, data_path)

  rds <- temp_rds_file()
  metamake_clean(input = px1, out_path = rds)

  # option 2
  pxmake_clean(input = rds, out_path = px2)

  # option 3
  metadata_rds <- readRDS(rds)

  pxmake_clean(metadata_rds$metadata, px3, metadata_rds$data)

  expect_equal_lines(px1, px2)
  expect_equal_lines(px1, px3)
  expect_true(TRUE) #required to run
})

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
