test_that("pxmake runs without errors and creates a file", {
  test_file_creation <- function(table_name) {
    px_file <- temp_px_file()

    pxmake_clean(input = get_metadata_path(table_name),
                 out_path = px_file,
                 data = get_data_path(table_name)
                 )

    expect_true(file.exists(px_file))
  }

  test_file_creation("BEXLTALL")
  test_file_creation("BEXSTA")
  test_file_creation("FOTEST")
  test_file_creation("no_timeval_or_codes")
  test_file_creation("zero_heading")
  test_file_creation("zero_stub")
})
