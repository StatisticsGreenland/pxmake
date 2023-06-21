test_that("Error if no 'Data' sheet exists when no source data is provided", {
  expect_error(pxmake_clean(get_metadata_path("BEXSTA"),
                            out_path = temp_pxfile()
                            ),
               regexp = "Data"
               )
})

test_that("Error if multiple time vars in variable", {
  table_name <- "error_two_timevals"
  expect_error(pxmake_clean(input = get_metadata_path(table_name),
                            out_path = temp_pxfile()
                            ),
               regexp = "type=time"
               )
})

test_that("Error if add_totals is specified without xlsx metadata", {
  expect_error(pxmake_clean(temp_rds_file(),
                            temp_pxfile(),
                            add_totals = c("place of birth", "gender")
                            ),
               regexp = "add_totals"
               )
})