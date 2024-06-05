test_that("px file = px_save(px(px file))", {
  expect_px_px_save_preserves_everything("BEXSTA")
  expect_px_px_save_preserves_everything("FOTEST")
  expect_px_px_save_preserves_everything("no_timeval_or_codes")
  expect_px_px_save_preserves_everything("zero_heading")
  expect_px_px_save_preserves_everything("zero_stub")
})
