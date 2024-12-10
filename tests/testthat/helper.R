get_data_path <- function(table_name) {
  tables_with_data_in_excel <-
    c("FOTEST", "no_timeval_or_codes", "zero_heading", "zero_stub")

  tabels_with_data_in_parquet <- "BEXSTA_parquet"

  if (tolower(table_name) %in% tolower(tables_with_data_in_excel)) {
    NULL
  } else if (tolower(table_name) %in% tolower(tabels_with_data_in_parquet)) {
    test_path('fixtures', 'data', paste0(table_name, '.parquet'))
  } else {
    test_path('fixtures', 'data', paste0(table_name, '.rds'))
  }
}

get_classification_path <- function(name) {
  test_path('fixtures', 'classification', name)
}

classification_path <- function(name) {
  function() {
    return(get_classification_path(name = name))
  }
}

agg_10years_path      <- classification_path("10-years classes.agg")
agg_25years_path      <- classification_path("25-years classes.agg")
vs_age5_path          <- classification_path("Age5.vs")
vs_age5_strangely_formatted_path <- classification_path("Age5_strangely_formatted.vs")

get_metadata_path <- function(table_name) {
  test_path('fixtures', 'metadata', stringr::str_glue("metadata_{table_name}.xlsx"))
}

get_px_file_path <- function(table_name) {
  test_path('fixtures', 'px', paste0(table_name, '.px'))
}

get_pxjob_file_path <- function(table_name) {
  test_path('fixtures', paste0(table_name, '_pxjob.px'))
}

expect_equal_lines <- function(path1, path2) {
  lines1 <- readLines(path1)
  lines2 <- readLines(path2)

  expect_equal(lines1, lines2)
}

px_from_table_name <- function(table_name) {
  px(input = get_metadata_path(table_name),
     data = get_data_path(table_name)
     )
}

expect_px_px_save_preserves_everything <- function(x) {
  px1   <- temp_px_file()
  px2   <- temp_px_file()

  px_save(x, path = px1)

  px1 %>%
    px() %>%
    px_save(path = px2)

  expect_equal_lines(px1, px2)
}

#' Run pxjob and delete created files when environment is killed
pxjob_clean <- function(input, output, env = parent.frame()) {
  pxjob64Win::pxjob(input, output)

  withr::defer(envir = env, {
    file.remove(output)
  })
}

#' Run px() and px_save() for a specific table. Return path to file.
create_px_file <- function(table_name) {
  px_path <- temp_px_file()

  px(input = get_metadata_path(table_name),
     data =  get_data_path(table_name)
     ) %>%
    px_save(path = px_path)

  return(px_path)
}

expect_save_read_preserves_classification <- function(c) {
  tempdir <- temp_dir()
  px_save_classification(c, tempdir)

  c2 <-
    px_classification(vs_path = list.files(tempdir, pattern = ".*\\.vs", full.names = TRUE))

  expect_identical(c, c2)
}

return_c2 <- function(c) {
  tempdir <- temp_dir()
  px_save_classification(c, tempdir)

  c2 <-
    px_classification(vs_path = list.files(tempdir, pattern = ".*\\.vs", full.names = TRUE))

  return(c2)
}
