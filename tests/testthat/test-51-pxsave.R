test_that("px_save basic functionality", {
  x <- px(get_data_path("BEXSTA"))

  # Runs without errors
  px_save(x, temp_xlsx_file())
  px_save(x, temp_px_file())
  px_save(x, temp_r_file())

  px(population_gl) %>%
    px_save(path = temp_r_file(), data_path = temp_rds_file())

  px(population_gl) %>%
    px_save(path = temp_r_file(), data_path = temp_parquet_file())

  expect_error(px_save(x), regexp = 'argument "path" is missing')
  expect_error(px_save(x, temp_rds_file()), regexp = "Argument 'path'")
})

test_that("px_save save_data and data_path arguments works", {
  df <- readRDS(get_data_path("BEXLTALL"))

  set.seed(1)

  df_large <-
    purrr::map(1:20, ~ df) %>%
    dplyr::bind_rows() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%  #add to avoid duplicated data
    dplyr::relocate(row_number)

  x <- px(df)

  x$data <- df_large

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
               regexp = "'data_path' can only be used if 'path' is an .xlsx or .R file"
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
