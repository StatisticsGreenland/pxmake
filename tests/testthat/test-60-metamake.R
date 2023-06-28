test_that("px file = pxmake(metamake(px file))", {
  expect_metamake_and_pxmake_cancel_out("BEXSTA")
  expect_metamake_and_pxmake_cancel_out("FOTEST")
  expect_metamake_and_pxmake_cancel_out("no_timeval_or_codes")
})

test_that("pxmake and metamake creates the same rds object", {
  px <- temp_px_file()

  rds_pxmake   <- pxmake_clean(get_metadata_path("FOTEST"), px)
  rds_metamake <- metamake_clean(px)

  expect_equal_rds(rds_pxmake, rds_metamake)
})

