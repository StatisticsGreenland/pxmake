get_data_path <- function(table_name) {
  tables_with_data_in_excel <-
    c("FOTEST", "no_timeval_or_codes", "zero_heading", "zero_stub")

  if (tolower(table_name) %in% tolower(tables_with_data_in_excel)) {
    NULL
  } else {
    test_path('fixtures', 'data', paste0(table_name, '.rds'))
  }
}

get_metadata_path <- function(table_name) {
  test_path('fixtures', 'metadata', stringr::str_glue("metadata_{table_name}.xlsx"))
}

get_px_file_path <- function(table_name) {
  test_path('fixtures', 'px', paste0(table_name, '.px'))
}

get_pxjob_file_path <- function(table_name) {
  test_path('fixtures', paste0(table_name, '_pxjob.px'))
}


temp_dir <- function() {
  path <- tempfile()
  dir.create(path)
  return(path)
}

expect_equal_lines <- function(path1, path2) {
  lines1 <- readLines(path1)
  lines2 <- readLines(path2)

  expect_equal(lines1, lines2)
}

expect_px_pxsave_preserves_everything <- function(table_name) {
  px1   <- temp_px_file()
  px2   <- temp_px_file()

  px(input = get_metadata_path(table_name),
     data = get_data_path(table_name)
     ) %>%
    pxsave(path = px1)

  px1 %>%
    px() %>%
    pxsave(path = px2)

  expect_equal_lines(px1, px2)
}

#' Run pxjob and delete created files when environment is killed
pxjob_clean <- function(input, output, env = parent.frame()) {
  pxjob64Win::pxjob(input, output)

  withr::defer(envir = env, {
    file.remove(output)
  })
}

#' Run px() and pxsave() for a specific table. Return path to file.
create_px_file <- function(table_name) {
  px_path <- temp_px_file()

  px(input = get_metadata_path(table_name),
     data =  get_data_path(table_name)
     ) %>%
    pxsave(path = px_path)

  return(px_path)
}
