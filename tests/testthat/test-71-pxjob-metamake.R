test_that("px file and pxmake(metamake(px file)) are equivalent", {
  # Some files don't give an exact match in the px-file because there can be
  # subtle differences in formatting. PxJob is run on these file to test
  # that they are equivalent.
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  run_metamake_pxmake_pxjob_and_compare <- function(table_name) {
    px1    <- get_px_filepath(table_name)
    px2    <- temp_px_file() # pxjob requires the .px extension
    meta1  <- temp_xlsx_file()
    pxjob1 <- temp_px_file()
    pxjob2 <- temp_px_file()

    metamake_clean(px1, meta1)
    pxmake_clean(meta1, px2)

    pxjob_clean(input = px1, output = pxjob1)
    pxjob_clean(input = px2, output = pxjob2)

    #' Read pxjob file and remove lines that doesn't need to be equal
    read_pxjobfile <- function(path) {
      lines <-
        path %>%
        readLines() %>%
        stringr::str_subset("^VARIABLECODE.+", negate = TRUE)

      if (table_name %in% c("no_timeval_or_codes2")) {
        lines <- stringr::str_subset(lines, "^CODES.+", negate = TRUE)
      }

      return(lines)
    }

    expect_equal(read_pxjobfile(pxjob1), read_pxjobfile(pxjob2))
  }

  run_metamake_pxmake_pxjob_and_compare("SOXATI4")
  run_metamake_pxmake_pxjob_and_compare("BEXSTA_windows_1252")
  run_metamake_pxmake_pxjob_and_compare("no_timeval_or_codes2")

  # Turn on when support for CELLNOTEX is added (issue #101)
  # run_metamake_pxmake_pxjob_and_compare("TUX01")
})
