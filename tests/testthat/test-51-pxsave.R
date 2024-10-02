test_that("px_save basic functionality", {
  x <- px(get_data_path("BEXSTA"))

  # Runs without errors
  px_save(x, temp_xlsx_file())
  px_save(x, temp_px_file())

  expect_error(px_save(x), regexp = 'argument "path" is missing')
  expect_error(px_save(x, temp_rds_file()), regexp = "Argument 'path'")
  expect_error(px_save(list(), temp_px_file()), regexp = "class 'px'")
})

test_that("px_save save_data and data_path arguments works", {
  x <- px(get_data_path("BEXLTALL"))
  x$data <- rbind(x$data, x$data)

  expect_error(px_save(x, temp_xlsx_file()), regexp = "too many rows")

  temp_xlsx <- temp_xlsx_file()
  px_save(x = x, path = temp_xlsx, save_data = FALSE)
  expect_true(! excel_sheet_exists('Data', temp_xlsx))

  expect_error(px_save(x = x, path = temp_px_file(), save_data = FALSE),
               regexp = "'save_data' can only be"
               )

  expect_error(px_save(x = x, path = temp_xlsx_file(), save_data = FALSE, data_path = temp_rds_file()),
               regexp = "'data_path' can only be used if 'save_data' is TRUE"
               )

  expect_error(px_save(x = x, path = temp_px_file(), data_path = temp_rds_file()),
               regexp = "'data_path' can only be used if 'path' is an .xlsx file"
               )

  expect_error(px_save(x = x, path = temp_xlsx_file(), data_path = temp_px_file()),
               regexp = "'data_path' must be a path to an .rds or .parquet file"
               )

  expect_error(px_save(x = x, path = temp_xlsx_file(), save_data = "maybe"),
               regexp = "'save_data' must be TRUE or FALSE")

  temp_rds <- temp_rds_file()
  px_save(x = x, path = temp_xlsx_file(), data_path = temp_rds)

  expect_true(! excel_sheet_exists('Data', temp_xlsx))
  expect_identical(x$data, readRDS(temp_rds))
})
