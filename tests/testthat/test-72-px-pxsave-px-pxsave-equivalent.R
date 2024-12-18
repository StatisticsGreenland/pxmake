test_that("that PX-file doesn't change second time through px -> px_save", {
  expect_px_save_does_not_change_on_second_pass <- function(table_name) {
    px_in  <- get_px_file_path(table_name)
    px_out1 <- temp_px_file()
    px_out2 <- temp_px_file()

    px_in %>%
      px() %>%
      px_save(path = px_out1)

    px_out1 %>%
      px() %>%
      px_save(path = px_out2)

    px_file_as_lines <- function(path) {
      lines <-
        path %>%
        readLines()

      return(lines)
    }

    expect_equal(px_file_as_lines(px_out1), px_file_as_lines(px_out2))
  }

  expect_px_save_does_not_change_on_second_pass("BEXSTA_windows_1252")
  expect_px_save_does_not_change_on_second_pass("CONTVARIABLE")
  expect_px_save_does_not_change_on_second_pass("CONTVARIABLE_multiple_languages")
  expect_px_save_does_not_change_on_second_pass("no_timeval_or_codes2")
  expect_px_save_does_not_change_on_second_pass("SOXATI4")
  expect_px_save_does_not_change_on_second_pass("TUX01")
})
