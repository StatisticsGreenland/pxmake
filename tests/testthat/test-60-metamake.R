test_that("px file = pxmake(metamake(px file))", {
  expect_metamake_and_pxmake_cancel_out("BEXSTA")
  expect_metamake_and_pxmake_cancel_out("FOTEST")
  expect_metamake_and_pxmake_cancel_out("no_timeval_or_codes")
  expect_metamake_and_pxmake_cancel_out("zero_heading")
  expect_metamake_and_pxmake_cancel_out("zero_stub")
})

test_that("pxmake and metamake creates the same rds object", {
  px <- temp_px_file()

  rds_pxmake   <- pxmake_clean(get_metadata_path("FOTEST"), px)
  rds_metamake <- metamake_clean(px)

  expect_equal_rds(rds_pxmake, rds_metamake)
})

test_that("metamake accepts a data frame as input", {
  metamake(input = women)
  expect_true(TRUE)
})
