test_that("Error if no 'Data' sheet exists when no source data is provided", {
  expect_error(pxmake(metadata_path = get_metadata_path("BEXSTA"),
                      pxfile_path = "",
                      source_data = NULL
                      ),
               regexp = "Data"
               )
})

test_that("Error if multiple time vars in variable", {
  table_name <- "error_two_timevals"
  expect_error(pxmake(metadata_path = get_metadata_path(table_name),
                      pxfile_path = '',
                      source_data = NULL
                      ),
               regexp = "type=time"
               )


})
