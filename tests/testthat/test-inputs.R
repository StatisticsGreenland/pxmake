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

test_that("metamake can take 3 types of input and give the same result", {
  meta <- get_metadata_path('FOTEST')
  px   <- temp_pxfile()
  rds  <- temp_rds_file()

  pxmake_clean(meta, rds)
  pxmake_clean(meta, px)

  rds1 <- temp_rds_file()
  rds2 <- temp_rds_file()
  rds3 <- temp_rds_file()

  # option 1
  metamake_clean(px, rds1)

  # option 2
  metamake_clean(rds, rds2)

  # option 3
  metamake_clean(readRDS(rds), rds3)

  expect_equal(readRDS(rds1), readRDS(rds2))
  expect_equal(readRDS(rds1), readRDS(rds3))
})
