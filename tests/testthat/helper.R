get_data_path <- function(table_name) {
  tables_with_data_in_excel <-
    c("FOTEST", "no_timeval_or_codes", "zero_heading", "zero_stub")

  if (table_name %in% tables_with_data_in_excel) {
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

expect_equal_lines <- function(path1, path2) {
  lines1 <- readLines(path1)
  lines2 <- readLines(path2)

  expect_equal(lines1, lines2)
}

#' Compare rds but don't require same sort order for data
expect_equal_rds <- function(rds1, rds2) {
  expect_equal(rds1$metadata, rds2$metadata)

  expect_equal(dplyr::arrange_all(rds1$data),
               dplyr::arrange_all(rds2$data)
  )
}

#' Metamake is the inverse of pxamke
expect_metamake_and_pxmake_cancel_out <- function(table_name) {
  px1   <- temp_px_file()
  meta1 <- get_metadata_path(table_name)
  data1 <- get_data_path(table_name)
  px2   <- temp_px_file()
  meta2 <- temp_xlsx_file()

  pxmake_clean(meta1, px1, data1)
  metamake_clean(px1, meta2)
  pxmake_clean(meta2, px2)

  expect_equal_lines(px1, px2)
}

#' Run pxmake and delete created files when environment is killed
pxmake_clean <- function(input,
                         out_path,
                         data = NULL,
                         add_totals = NULL,
                         env = parent.frame()) {
  rds <- pxmake(input, out_path, data, add_totals)

  return(invisible(rds))

  withr::defer(envir = env, {
    Sys.sleep(1)
    if (!is.null(out_path)) {
      file.remove(out_path)
    }
  })
}

#' Run metamake and delete created files when environment is killed
metamake_clean <- function(input,
                           out_path = NULL,
                           data_path = NULL,
                           env = parent.frame()) {

  rds <- metamake(input, out_path, data_path)

  return(invisible(rds))

  withr::defer(envir = env, {
    Sys.sleep(.1)

    if (!is.null(out_path)) {
      file.remove(out_path)
    }

    if (!is.null(data_table_path)) {
      file.remove(data_table_path)
    }
  }
  )
}

#' Run pxjob and delete created files when environment is killed
pxjob_clean <- function(input, output, env = parent.frame()) {
  pxjob64Win::pxjob(input, output)

  withr::defer(envir = env, {
    file.remove(output)
  })
}

#' Run pxmake for a specific table. Return path to file.
create_px_file <- function(table_name) {
  px_path <- temp_px_file()

  pxmake_clean(get_metadata_path(table_name),
               px_path,
               get_data_path(table_name),
               env = parent.frame(n=1)
               )

  return(px_path)
}
