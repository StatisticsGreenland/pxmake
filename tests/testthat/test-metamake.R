test_that("metamake is the inverse of pxmake", {
  run_metmake_pxmake_and_compare <- function(table_name) {
    px_source    <- get_pxfile_path(table_name)
    metadata_out <- get_metadata_path(paste0(table_name, "_by_metamake"))
    px_out       <- get_pxfile_path(paste0(table_name, "_metamake_pxmake"))

    metamake(px_source, metadata_out)

    pxmake(metadata_out, px_out)

    output <- readLines(px_source)
    expect <- readLines(px_out)

    expect_equal(output, expect)
  }

  run_metmake_pxmake_and_compare("BEXLTALL")
  run_metmake_pxmake_and_compare("BEXSTA")
  run_metmake_pxmake_and_compare("FOTEST")
})
