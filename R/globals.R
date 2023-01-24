# File and directory paths

data_dir     <- file.path('data')
data_raw_dir <- file.path('data-raw')
metadata_dir <- file.path(data_raw_dir, 'metadata')

# statistikbank
bexstatest_rds_path <- file.path(data_raw_dir, "BEXSTATEST.rds")
# sas
bexstatest2_rds_path <- file.path(data_raw_dir, "BEXSTATEST2.rds")
# R
bexltall_rds_path <- file.path(data_raw_dir, "BEXLTALL.rds")
#bexltreg_rds_path <- file.path(data_raw_dir, "BEXLTREG.rds")
# Excel
excelsample_rds_path <- file.path(data_raw_dir, "EXCELSAMPLE.rds")

FOtest_rds_path <- file.path(data_raw_dir, "FOTEST.rds")


r_dir <- file.path('R')
helper_functions_file_path <- file.path(r_dir, 'helper_functions.R')

get_excel_metadata_path <- function(table_name) {
  file.path(metadata_dir, str_glue("metadata_{str_to_upper(table_name)}.xlsx"))
}

get_source_data_path <- function(table_name) {
  file.path(data_raw_dir, str_glue("{str_to_upper(table_name)}.rds"))
}

get_px_file_path <- function(table_name) {
  file.path(data_dir, str_glue("{str_to_upper(table_name)}.px"))
}

