test_that("MAP is added in variables2", {
  x <- px(get_px_file_path("MAP"))

  map_rows_in_table_2 <-
    x$table2 %>%
    dplyr::filter(keyword == "MAP")

  expect_identical(map_rows_in_table_2,
                   dplyr::filter(map_rows_in_table_2, FALSE)
                   )

  map_rows_in_variables2 <-
    x$variables2 %>%
    tidyr::drop_na(map)

  expect_true(nrow(map_rows_in_variables2) > 0)
})
