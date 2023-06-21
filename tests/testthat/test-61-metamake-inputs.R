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
