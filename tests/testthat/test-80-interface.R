test_that("Source data variable names are preserved",{
  table_name <- "FOTEST"
  px1   <- temp_px_file()
  meta1 <- get_metadata_path(table_name)
  meta2 <- temp_xlsx_file()

  pxmake_clean(meta1, px1)
  metamake_clean(px1, meta2)

  get_data_sheet_variable_names <- function(path) {
    path %>%
      readxl::read_excel(sheet = "Data") %>%
      names()
  }

  expect_equal(get_data_sheet_variable_names(meta1),
               get_data_sheet_variable_names(meta2)
               )
})
