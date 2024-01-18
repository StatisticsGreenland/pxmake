test_that("Error if no 'Data' sheet exists when no data argument is provided", {
  expect_error(px(get_metadata_path("BEXSTA")), regexp = "Data")
})

test_that("Error if multiple time vars in variable", {
  expect_error(px(input = get_metadata_path("error_two_timevals")),
               regexp = "time"
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
