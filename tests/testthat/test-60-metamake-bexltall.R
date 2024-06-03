# BEXLTALL runs in its own file because it takes a long time
test_that("px file = pxsave(px(px file))", {
  expect_px_pxsave_preserves_everything("BEXLTALL")
})
