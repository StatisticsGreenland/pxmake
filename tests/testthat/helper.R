get_source_data_path <- function(table_name) {
  test_path('fixtures', 'data', paste0(table_name, '.rds'))
}

get_metadata_path <- function(table_name) {
  test_path('fixtures', 'metadata', stringr::str_glue("metadata_{table_name}.xlsx"))
}

get_pxfile_path <- function(table_name) {
  test_path('px', paste0(table_name, '.px'))
}

get_pxjobfile_path <- function(table_name) {
  test_path('px', paste0(table_name, '_pxjob.px'))
}
