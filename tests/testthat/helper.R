get_source_data_path <- function(table_name) {
  if (table_name %in% c("FOTEST", "no_timeval_or_codes")) {
    NULL
  } else {
    test_path('fixtures', 'data', paste0(table_name, '.rds'))
  }
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

expect_equal_lines <- function(path1, path2) {
  lines1 <- readLines(path1)
  lines2 <- readLines(path2)

  expect_equal(lines1, lines2)
}

#' Version of pxmake that deletes created file once finished
pxmake_clean <- function(excel_metadata_path,
                         px_path,
                         data_table = NULL,
                         add_totals = NULL,
                         env = parent.frame()) {
  pxmake(excel_metadata_path, px_path, data_table, add_totals)

  withr::defer(envir = env, {
    Sys.sleep(1)
    file.remove(px_path)
    }
  )
}

metamake_clean <- function(px_path,
                           xlsx_path,
                           rds_data_path = NULL,
                           overwrite_xlsx = TRUE,
                           env = parent.frame()) {

  metamake(px_path, xlsx_path, rds_data_path, overwrite_xlsx)

  withr::defer(envir = env, {
    Sys.sleep(.1)
    file.remove(xlsx_path)

    if (! is.null(rds_data_path)) {
      file.remove(rds_data_path)
    }
  }
  )
}

pxjob_clean <- function(input, output, env = parent.frame()) {
  pxjob64Win::pxjob(input, output)

  withr::defer(envir = env, {
    file.remove(output)
  })
}

