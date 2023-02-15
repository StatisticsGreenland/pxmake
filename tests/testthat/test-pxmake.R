get_source_data_path <- function(table_name) {
  test_path('fixtures', 'data', paste0(table_name, '.rds'))
}

get_metadata_path <- function(table_name) {
  test_path('fixtures', 'metadata', glue::glue("metadata_{table_name}.xlsx"))
}

get_pxfile_path <- function(table_name) {
  test_path('px', paste0(table_name, '.px'))
}

get_pxjobfile_path <- function(table_name) {
  test_path('px', paste0(table_name, '_pxjob.px'))
}


test_that("pxmake runs without errors and creates a file", {
  test_file_creation <- function(table_name) {
    if (file.exists(get_pxfile_path(table_name))) {
      file.remove(get_pxfile_path(table_name))
    }

    pxmake(get_source_data_path(table_name),
           get_metadata_path(table_name),
           get_pxfile_path(table_name)
           )

    expect_true(file.exists(get_pxfile_path(table_name)))
  }

  test_file_creation("BEXLTALL")
  test_file_creation("BEXSTA")
  test_file_creation("FOTEST")
})


test_that("px lines are valid", {
  keywords <- get_px_keywords() %>% dplyr::pull(keyword)

  valid_lines <-
    c(paste0("^", keywords, "[=\\[\\(]"), # keyword followed by [ ( or =
      '^[e[:digit:][:space:]"-.]+$',      # data lines
      '^;$'                               # last line of file
      )

  regex <- paste0(valid_lines, collapse = "|")

  get_invalid_lines <- function(table_name) {
    px_lines <-
      get_pxfile_path(table_name) %>%
      readLines()

    px_lines[stringr::str_detect(px_lines, regex, negate = TRUE)]
  }

  expect_equal(get_invalid_lines("BEXLTALL"), character(0))
  expect_equal(get_invalid_lines("BEXSTA"), character(0))
  expect_equal(get_invalid_lines("FOTEST"), character(0))
})


test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  run_pxjob <- function(table_name) {
    pxjob64Win::pxjob(input = get_pxfile_path(table_name),
                      output = get_pxjobfile_path(table_name)
                      )
  }

  expect_equal(run_pxjob("BEXLTALL"), 0)
  expect_equal(run_pxjob("BEXSTA"), 0)
  expect_equal(run_pxjob("FOTEST"), 0)
})

