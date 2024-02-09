test_that("px file = pxmake(metamake(px file))", {
  expect_metamake_and_pxmake_cancel_out("BEXSTA")
  expect_metamake_and_pxmake_cancel_out("FOTEST")
  expect_metamake_and_pxmake_cancel_out("no_timeval_or_codes")
  expect_metamake_and_pxmake_cancel_out("zero_heading")
  expect_metamake_and_pxmake_cancel_out("zero_stub")
})
