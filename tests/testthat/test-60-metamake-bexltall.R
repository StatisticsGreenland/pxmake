# BEXLTALL runs in its own file because it takes a long time
test_that("px file = pxmake(metamake(px file))", {
  expect_metamake_and_pxmake_cancel_out("BEXLTALL")
})
