# Test pxmake.
#
# The 3 main data tables asserts the following features:
#
# Headings
# - 1 heading (BEXLTALL, FOTEST)
# - 2 headings (BEXSTA)

# Languages
# - 2-3 languages (BEXSTA, BEXLTALL, FOTEST)
#
# Data
# - Data in 'Data' sheet in Excel (FOTEST)
# - Data in .rds file (BEXSTA, BEXLTALL)
#
# Other
# - >=2 STUBS (BEXSTA, BEXLTALL, FOTEST)
# - Data with groups (BEXLTALL)
# - Numeric variable type ('age' in BEXLTALL)
# - A value in 'Codelist' is not present in the data. (BEXLTALL)
#
# Time
# - Years (BEXSTA, BEXLTALL)
# - Quaters (FOTEST)
# - Data without a timeval (no_timeval)
#
# Long lines
# - VALUES longer than 256 characters (BEXLTALL)
# - NOTE longer than 256 characters (BEXSTA)

test_that("pxmake runs without errors and creates a file", {
  test_file_creation <- function(table_name) {
    if (file.exists(get_pxfile_path(table_name))) {
      file.remove(get_pxfile_path(table_name))
    }

    if (table_name %in% c("FOTEST", "no_timeval")) {
      source_data_path <- NULL
    } else {
      source_data_path <- get_source_data_path(table_name)
    }

    pxmake(get_metadata_path(table_name),
           get_pxfile_path(table_name),
           source_data_path
           )

    expect_true(file.exists(get_pxfile_path(table_name)))
  }

  test_file_creation("BEXLTALL")
  test_file_creation("BEXSTA")
  test_file_creation("FOTEST")
  test_file_creation("no_timeval")
})

test_that("pxmake accepts a data frame object", {
  table_name <- "BEXSTA"

  df <- readRDS(get_source_data_path(table_name))

  pxmake(get_metadata_path(table_name),
         get_pxfile_path(paste0(table_name, "_with_df")),
         source_data_path = df
         )

  expect <- readLines(get_pxfile_path(paste0(table_name)))
  output <- readLines(get_pxfile_path(paste0(table_name, "_with_df")))

  expect_equal(expect, output)
})

test_that("timevals are added", {
  pxfile_has_timeval <- function(table_name) {
    px_lines <-
      table_name %>%
      get_pxfile_path() %>%
      readLines()

    timeval_lines <- px_lines[stringr::str_detect(px_lines, "^TIMEVAL")]

    length(timeval_lines) > 0
  }

  expect_true(pxfile_has_timeval("BEXLTALL"))
  expect_true(pxfile_has_timeval("BEXSTA"))
  expect_true(pxfile_has_timeval("FOTEST"))
})

test_that("Codes are defined for all values", {
  expect_code_and_values_match <- function(table_name) {
    px_lines <-
      table_name %>%
      get_pxfile_path() %>%
      readLines()

    value_lines <- px_lines[stringr::str_detect(px_lines, '^VALUES')]
    code_lines  <- px_lines[stringr::str_detect(px_lines, '^CODES')]

    # Equal number of variables
    expect_equal(length(value_lines), length(code_lines))

    # Equal number of values for each variable
    expect_equal(stringr::str_count(value_lines, '","'),
                 stringr::str_count(code_lines,  '","')
    )
  }

  expect_code_and_values_match("BEXLTALL")
  expect_code_and_values_match("BEXSTA")
  expect_code_and_values_match("FOTEST")
})

test_that("px lines are valid", {
  keywords <- get_px_keywords() %>% dplyr::pull(keyword)

  valid_lines <-
    c(paste0("^", keywords, "[=\\[\\(]"), # keyword followed by [ ( or =
      '^".+[",;]$',                       # continued lines (when previous is longer than 256)
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

test_that("header lines doesn't exceed 256 characters", {
  file_does_not_have_too_long_lines <- function(table_name) {
    px_lines <-
      get_pxfile_path(table_name) %>%
      readLines()

    data_line_index <- stringr::str_which(px_lines, '^DATA=$')

    long_lines <-
      tibble::tibble(line = px_lines[1:data_line_index]) %>%
      dplyr::mutate(text = stringr::str_extract(line, "(?<==).+"),
                    length = nchar(text)
      ) %>%
      dplyr::filter(length > 256)

    expect_equal(long_lines, dplyr::filter(long_lines, FALSE))
  }

  file_does_not_have_too_long_lines('BEXSTA')
  file_does_not_have_too_long_lines('FOTEST')
  file_does_not_have_too_long_lines('BEXLTALL')
})

test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  pxjob_runs_without_erros <- function(table_name) {
    0 == pxjob64Win::pxjob(input = get_pxfile_path(table_name),
                           output = get_pxjobfile_path(table_name)
                           )
  }

  expect_true(pxjob_runs_without_erros("BEXLTALL"))
  expect_true(pxjob_runs_without_erros("BEXSTA"))
  expect_true(pxjob_runs_without_erros("FOTEST"))
})
