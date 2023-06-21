test_that("pxfile = pxmake(metamake(pxfile))", {
  expect_metamake_and_pxmake_cancel_out("BEXSTA")
  expect_metamake_and_pxmake_cancel_out("FOTEST")
  expect_metamake_and_pxmake_cancel_out("no_timeval_or_codes")
})

