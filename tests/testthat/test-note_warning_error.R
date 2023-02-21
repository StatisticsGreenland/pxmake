test_that("Error if no 'Data' sheet exists when no source data is provided", {
  expect_error(pxmake(metadata_path = get_metadata_path("BEXSTA"),
                      pxfile_path = "",
                      source_data_path = NULL
                      ),
               regexp = "Data"
               )
})
