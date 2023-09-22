test_that("micromake runs without errors and creates px files", {

  test_file_creation <- function(table_name) {
    rds <- pxmake_clean(input = get_metadata_path(table_name),
                        data  = get_data_path(table_name)
                        )

    temp_dir <- temp_dir()

    micromake(rds, out_dir = temp_dir)

    expect_true(length(list.files(temp_dir)) > 0)
  }

  test_file_creation("BEXSTA")
  test_file_creation("FOTEST")
  test_file_creation("no_timeval_or_codes")
  test_file_creation("zero_heading")
  test_file_creation("zero_stub")
})
