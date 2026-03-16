test_that("PX-file and px_save(px(PX-file), path='*.px') are equivalent", {
  # Some files don't give an exact match in the PX-file because there can be
  # subtle differences in formatting. PxJob is run on these file to test
  # that they are equivalent.
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  run_px_px_save_and_expect_equal <- function(table_name) {
    px_in  <- get_px_file_path(table_name)
    px_out <- temp_px_file()

    px_in %>%
      px() %>%
      px_save(path = px_out)

    pxjob_clean(input = px_in,  output = pxjob_in  <- temp_px_file())
    pxjob_clean(input = px_out, output = pxjob_out <- temp_px_file())

    px_file_as_lines <- function(path) {
      ignore_keywords_regex <-
        c("VARIABLECODE") %>%
        paste0("^", ., ".+") %>%
        paste(collapse = "|")

      lines <-
        path %>%
        readLines() %>%
        stringr::str_subset(ignore_keywords_regex, negate = TRUE)

      if (table_name %in% c("no_timeval_or_codes2")) {
        lines <- stringr::str_subset(lines, "^CODES.+", negate = TRUE)
      }

      if (table_name %in% c("CONTVARIABLE_multiple_languages")) {
        lines <- stringr::str_subset(lines, '^VARIABLE-TYPE.+Time"', negate = TRUE)
        lines <- stringr::str_subset(lines, "META-ID", negate = TRUE)
      }

      return(lines)
    }

    expect_equal(px_file_as_lines(pxjob_in), px_file_as_lines(pxjob_out))
  }

  run_px_px_save_and_expect_equal("BEXSTA_windows_1252")
  run_px_px_save_and_expect_equal("CONTVARIABLE")
  run_px_px_save_and_expect_equal("CONTVARIABLE_multiple_languages")
  run_px_px_save_and_expect_equal("no_timeval_or_codes2")
  run_px_px_save_and_expect_equal("SOXATI4")
  run_px_px_save_and_expect_equal("TUX01")
})
