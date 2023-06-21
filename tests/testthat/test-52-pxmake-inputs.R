test_that("pxmake can take 4 types of input and give the same result", {
  px1 <- temp_pxfile()
  px2 <- temp_pxfile()
  px3 <- temp_pxfile()
  px4 <- temp_pxfile()

  table_name <- "BEXSTA"
  xlsx       <- get_metadata_path(table_name)
  data_table <- get_source_data_path(table_name)

  # option 1 (see ?pxmake)
  pxmake_clean(xlsx, px1, data_table)

  rds <- temp_rds_file()
  metamake_clean(px1, rds)

  # option 2
  pxmake_clean(rds, px2)

  # option 3
  metadata_rds <- readRDS(rds)

  pxmake_clean(metadata_rds$metadata, px3, metadata_rds$data_table)

  # option 4
  pxmake_clean(metadata_rds, px4)

  expect_equal_lines(px1, px2)
  expect_equal_lines(px1, px3)
  expect_equal_lines(px1, px4)
  expect_true(TRUE) #required to run
})

test_that("pxmake accepts a data frame object", {
  table_name <- "BEXSTA"

  px1   <- temp_pxfile()
  meta1 <- get_metadata_path(table_name)
  data1 <- get_source_data_path(table_name)
  px2   <- temp_pxfile()
  df    <- readRDS(get_source_data_path(table_name))

  pxmake_clean(meta1, px1, data_table = data1)
  pxmake_clean(meta1, px2, data_table = df)

  expect_equal_lines(px1, px2)
})
