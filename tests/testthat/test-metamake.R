# test_that("metamake runs without errors", {
#   table_name <- "metamake"
#
#   metamake(get_pxfile_path(table_name),
#            get_metadata_path(table_name)
#            )
#
#   expect(TRUE)
# })

test_that("metamake is the inverse of pxmake", {
  table_name <- "metamake"

  metamake(get_pxfile_path(table_name),
           get_metadata_path(table_name)
           )

  pxmake(get_metadata_path(table_name),
         get_pxfile_path("metamake_clone")
         )

  output <- readLines(get_pxfile_path("metamake_clone"))
  expect <- get_pxfile_path("metamake")

  expect_equal(output, expect)
})
