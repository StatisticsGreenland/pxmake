test_that("metamake is the inverse of pxmake", {
  px_source    <- get_pxfile_path("bexsta")
  metadata_out <- get_metadata_path("bexsta_by_metamake")
  px_out       <- get_pxfile_path("BEXSTA_clone")

  metamake(px_source, metadata_out)

  pxmake(metadata_out, px_out)

  output <- readLines(px_source)
  expect <- readLines(px_out)

  expect_equal(output, expect)
})
