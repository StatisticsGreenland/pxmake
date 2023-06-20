test_that("pxmake can make xlsx to rds, and rds to px",{
  table_name <- "BEXSTA"

  metadata <- get_metadata_path(table_name)
  data_table <- get_source_data_path(table_name)

  px1 <- temp_pxfile()
  px2 <- temp_pxfile()
  rds <- temp_rds_file()

  # xlsx to rds to px
  pxmake_clean(metadata, rds, data_table)
  pxmake_clean(rds, px1)

  # xlsx direct to px
  pxmake_clean(metadata, px2, data_table)

  expect_equal_lines(px1, px2)
  expect_true(TRUE) # otherwise test is skipped
})

test_that('metmake can make px to rds, and rds to xlsx', {
  px0   <- get_pxfile_path('BEXSTA_windows_1252')
  px1   <- temp_pxfile()
  px2   <- temp_pxfile()
  rds   <- temp_rds_file()
  xlsx1 <- temp_xlsx_file()
  xlsx2 <- temp_xlsx_file()

  # px to rds to xlsx
  metamake_clean(px0, rds)
  metamake_clean(rds, xlsx1)

  # px direct to xlsx
  metamake_clean(px0, xlsx2)

  pxmake_clean(xlsx1, px1)
  pxmake_clean(xlsx2, px2)

  expect_equal_lines(px1, px2)
  expect_true(TRUE) # otherwise test is skipped
})

test_that('pxmake can make rds to rds', {
  xlsx <- get_metadata_path('FOTEST')
  rds1 <- temp_rds_file()
  rds2 <- temp_rds_file()

  pxmake_clean(xlsx, rds1)

  pxmake_clean(rds1, rds2)

  expect_equal(readRDS(rds1), readRDS(rds2))
})

test_that('metamake can make rds to rds', {
  # One-language file
  meta <- get_metadata_path('FOTEST')
  px   <- temp_pxfile()
  rds1  <- temp_rds_file()
  rds2  <- temp_rds_file()

  pxmake_clean(meta, rds1)

  metamake_clean(rds1, rds2)

  expect_equal_rds(readRDS(rds1), readRDS(rds2))
})
