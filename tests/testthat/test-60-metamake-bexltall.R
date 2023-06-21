# BEXLTALL runs in its own file because it takes a long time
test_that("pxfile = pxmake(metamake(pxfile))", {
  expect_metamake_and_pxmake_cancel_out("BEXLTALL")
})
