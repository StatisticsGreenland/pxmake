# test_that("metamake can take 3 types of input and give the same result", {
#   meta <- get_metadata_path('FOTEST')
#   px   <- temp_px_file()
#   rds  <- temp_rds_file()
#
#   pxmake_clean(meta, rds)
#   pxmake_clean(meta, px)
#
#   rds1 <- temp_rds_file()
#   rds2 <- temp_rds_file()
#   rds3 <- temp_rds_file()
#
#   # option 1
#   metamake_clean(px, rds1)
#
#   # option 2
#   # metamake_clean(rds, rds2)
#
#   # option 3
#   metamake_clean(readRDS(rds), rds3)
#
#   rds  <- readRDS(rds)
#   rds1 <- readRDS(rds1)
#   # rds2 <- readRDS(rds2)
#   rds3 <- readRDS(rds3)
#
#   sort_and_expect_equal <- function(rds_a, rds_b) {
#     expect_equal(lapply(rds_a, dplyr::arrange_all),
#                  lapply(rds_b, dplyr::arrange_all)
#                  )
#   }
#
#   sort_and_expect_equal(rds, rds2)
#   sort_and_expect_equal(rds1, rds2)
#   sort_and_expect_equal(rds1, rds3)
#
#   # expect_equal(readRDS(rds1), readRDS(rds2))
#   # expect_equal(readRDS(rds1), readRDS(rds3))
# })
