# BEXLTALL runs in its own file because it takes a long time
test_that("PX-file = px_save(px(PX-file))", {
  expect_px_px_save_preserves_everything(px_from_table_name("BEXLTALL"))
})
