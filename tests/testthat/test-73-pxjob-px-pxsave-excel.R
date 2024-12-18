test_that("px(PX-file) and px(px_save(px(PX-file), path = excel)) are equivalent", {
  # Some files don't give an exact match in the PX-file because there can be
  # subtle differences in formatting. PxJob is run on these file to test
  # that they are equivalent.
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  run_px_px_save_excel_pxjob_and_compare <- function(table_name) {
    px1    <- get_px_file_path(table_name)
    excel1 <- temp_xlsx_file()
    px2    <- temp_px_file()
    pxjob1 <- temp_px_file()
    pxjob2 <- temp_px_file()

    px1 %>%
      px() %>%
      px_save(path = excel1)

    excel1 %>%
      px() %>%
      px_save(path = px2)

    pxjob_clean(input = px1, output = pxjob1)
    pxjob_clean(input = px2, output = pxjob2)

    #' Read pxjob file and remove lines that doesn't need to be equal
    read_pxjobfile <- function(path) {
      lines <-
        path %>%
        readLines() %>%
        # Improve when implementing #163
        stringr::str_subset("^VARIABLECODE.+", negate = TRUE) %>%
        stringr::str_subset("^META-ID.+", negate = TRUE) %>%
        {if (table_name == "CONTVARIABLE_multiple_languages") {
          .
          stringr::str_subset(., '^VARIABLE-TYPE.+Time"', negate = TRUE)
        } else {
          .
        }}

      if (table_name %in% c("no_timeval_or_codes2")) {
        lines <- stringr::str_subset(lines, "^CODES.+", negate = TRUE)
      }

      if (table_name %in% c("CONTVARIABLE")) {
        lines <- stringr::str_subset(lines, "^CODES\\(.year.\\).+", negate = TRUE)
      }

      return(lines)
    }

    expect_equal(read_pxjobfile(pxjob1), read_pxjobfile(pxjob2))
  }

  run_px_px_save_excel_pxjob_and_compare("SOXATI4")
  run_px_px_save_excel_pxjob_and_compare("BEXSTA_windows_1252")
  run_px_px_save_excel_pxjob_and_compare("no_timeval_or_codes2")
  run_px_px_save_excel_pxjob_and_compare("CONTVARIABLE")
  run_px_px_save_excel_pxjob_and_compare("CONTVARIABLE_multiple_languages")
  run_px_px_save_excel_pxjob_and_compare("TUX01")
})
