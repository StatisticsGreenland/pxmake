test_that("Error if no 'Data' sheet exists when no source data is provided", {
  expect_error(pxmake_clean(excel_metadata_path = get_metadata_path("BEXSTA"),
                            px_path = "",
                            data_table = NULL
                            ),
               regexp = "Data"
               )
})

test_that("Error if multiple time vars in variable", {
  table_name <- "error_two_timevals"
  expect_error(pxmake_clean(excel_metadata_path = get_metadata_path(table_name),
                            px_path = '',
                            data_table = NULL
                            ),
               regexp = "type=time"
               )


})
