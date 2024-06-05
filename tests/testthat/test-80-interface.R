test_that("Source data variable names are preserved",{
  table_name <- "FOTEST"
  excel1 <- get_metadata_path(table_name)
  excel2 <- temp_xlsx_file()

  excel1 %>%
    px() %>%
    px_save(path = excel2)

  get_data_sheet_variable_names <- function(path) {
    path %>%
      readxl::read_excel(sheet = "Data") %>%
      names()
  }

  expect_equal(get_data_sheet_variable_names(excel1),
               get_data_sheet_variable_names(excel2)
               )
})
