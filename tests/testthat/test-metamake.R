test_that("px = pxmake(metamake(pxfile))", {
  run_metamake_pxmake_and_compare <- function(table_name) {
    px_source    <- get_pxfile_path(table_name)
    metadata_out <- get_metadata_path(paste0(table_name, "_by_metamake"))
    px_out       <- get_pxfile_path(paste0(table_name, "_metamake_pxmake"))

    metamake(px_source, metadata_out)

    pxmake(metadata_out, px_out)

    output <- readLines(px_source)
    expect <- readLines(px_out)

    expect_equal(output, expect)
  }

  run_metamake_pxmake_and_compare("BEXSTA")
  run_metamake_pxmake_and_compare("FOTEST")
  run_metamake_pxmake_and_compare("BEXLTALL")
})

test_that("px and pxmake(metamake(pxfile)) are equivalent", {
  # Some files don't give an exact match in the px-file because there can be
  # subtle differences in formatting. PxJob is run on these file to test
  # that they are equivalent.
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  pxjob_runs_without_erros <- function(table_name) {
    0 == pxjob64Win::pxjob(input = get_pxfile_path(table_name),
                           output = get_pxjobfile_path(table_name)
    )
  }


  #run_metamake_pxmake_and_compare("TUX01")
})
