test_that("pxmake can take 4 types of input and give the same result", {
  px1 <- temp_px_file()
  px2 <- temp_px_file()
  px3 <- temp_px_file()
  px4 <- temp_px_file()

  table_name <- "BEXSTA"
  xlsx       <- get_metadata_path(table_name)
  data_path  <- get_data_path(table_name)

  # option 1 (see ?pxmake)
  pxmake_clean(xlsx, px1, data_path)

  rds <- temp_rds_file()
  metamake_clean(px1, rds)

  # option 2
  pxmake_clean(rds, px2)

  # option 3
  metadata_rds <- readRDS(rds)

  pxmake_clean(metadata_rds$metadata, px3, metadata_rds$data)

  # option 4
  pxmake_clean(metadata_rds, px4)

  expect_equal_lines(px1, px2)
  expect_equal_lines(px1, px3)
  expect_equal_lines(px1, px4)
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
