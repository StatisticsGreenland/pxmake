test_that("Error if no 'Data' sheet exists when no data argument is provided", {
  expect_error(pxmake_clean(get_metadata_path("BEXSTA"),
                            out_path = temp_px_file()
                            ),
               regexp = "Data"
               )
})

test_that("Error if multiple time vars in variable", {
  expect_error(pxmake_clean(input = get_metadata_path("error_two_timevals"),
                            out_path = temp_px_file()
                            ),
               regexp = "type=time"
               )
})

test_that("Error if add_totals is specified without xlsx metadata", {
  expect_error(pxmake_clean(temp_rds_file(),
                            temp_px_file(),
                            add_totals = c("place of birth", "gender")
                            ),
               regexp = "add_totals"
               )
})
