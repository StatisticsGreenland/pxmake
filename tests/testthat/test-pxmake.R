# Test pxmake.
#
# The 3 main data tables asserts the following features:
#
# Headings
# - 1 heading (BEXLTALL, FOTEST)
# - 2 headings (BEXSTA)

# Languages
# - 1 language (FOTEST)
# - 2-3 languages (BEXSTA, BEXLTALL)
# - Other main language than English (FOTEST)
#
# Data
# - Data in 'Data' sheet in Excel (FOTEST)
# - Data in .rds file (BEXSTA, BEXLTALL)
#
# Time
# - Years (BEXSTA, BEXLTALL)
# - Quaters (FOTEST)
# - Data without a timeval (no_timeval_or_codes)
#
# Encoding
# - utf-8 (BELTALL, BEXSTA, FOTEST)
# - Windows-1252 (BEXSTA_windows_1251)
#
# Long lines
# - VALUES longer than 256 characters (BEXLTALL)
# - NOTE longer than 256 characters (BEXSTA)
#
# Other
# - >=2 STUBS (BEXSTA, BEXLTALL, FOTEST)
# - Data with groups (BEXLTALL)
# - Numeric variable type ('age' in BEXLTALL)
# - A value in 'Codelist' is not present in the data. (BEXLTALL)
# - Data without codes (no_timeval_or_codes)
# - Variable names in source data are preserved by using VARIABLECODE (FOTEST)

test_that("pxmake runs without errors and creates a file", {
  test_file_creation <- function(table_name) {
    px_file <- temp_pxfile()

    pxmake_clean(get_metadata_path(table_name),
                 px_file,
                 get_source_data_path(table_name)
                 )

    expect_true(file.exists(px_file))
  }

  test_file_creation("BEXLTALL")
  test_file_creation("BEXSTA")
  test_file_creation("FOTEST")
  test_file_creation("no_timeval_or_codes")
})

test_that("pxmake accepts a data frame object", {
  table_name <- "BEXSTA"

  px1   <- temp_pxfile()
  meta1 <- get_metadata_path(table_name)
  data1 <- get_source_data_path(table_name)
  px2   <- temp_pxfile()
  df    <- readRDS(get_source_data_path(table_name))

  pxmake_clean(meta1, px1, data_table = data1)
  pxmake_clean(meta1, px2, data_table = df)

  expect_equal_lines(px1, px2)
})

test_that("timevals are added", {
  expect_that_pxfile_has_timeval <- function(table_name) {
    px_path <- temp_pxfile()

    pxmake_clean(get_metadata_path(table_name),
                 px_path,
                 get_source_data_path(table_name)
                 )

    px_lines <- readLines(px_path)

    timeval_lines <- px_lines[stringr::str_detect(px_lines, "^TIMEVAL")]

    expect_true(length(timeval_lines) > 0)
  }

  expect_that_pxfile_has_timeval("BEXLTALL")
  expect_that_pxfile_has_timeval("BEXSTA")
  expect_that_pxfile_has_timeval("FOTEST")
})

test_that("Codes are defined for all values", {
  expect_code_and_values_match <- function(table_name) {
    px_path <- temp_pxfile()

    pxmake_clean(get_metadata_path(table_name),
                 px_path,
                 get_source_data_path(table_name)
                 )

    px_lines <- readLines(px_path)

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

  expect_no_invalid_lines <- function(table_name) {
    px_path <- temp_pxfile()

    pxmake_clean(get_metadata_path(table_name),
                 px_path,
                 get_source_data_path(table_name)
                 )

    px_lines <- readLines(px_path)

    invalid_lines <- px_lines[stringr::str_detect(px_lines, regex, negate = TRUE)]

    expect_equal(invalid_lines, character(0))
  }

  expect_no_invalid_lines("BEXLTALL")
  expect_no_invalid_lines("BEXSTA")
  expect_no_invalid_lines("FOTEST")
})

test_that("header lines doesn't exceed 256 characters", {
  expect_no_lines_long_lines <- function(table_name) {
    px_path <- temp_pxfile()

    pxmake_clean(get_metadata_path(table_name),
                 px_path,
                 get_source_data_path(table_name)
                 )

    px_lines <- readLines(px_path)

    data_line_index <- stringr::str_which(px_lines, '^DATA=$')

    long_lines <-
      tibble::tibble(line = px_lines[1:data_line_index]) %>%
      dplyr::mutate(text = stringr::str_extract(line, "(?<==).+"),
                    length = nchar(text)
                    ) %>%
      dplyr::filter(length > 256)

    expect_equal(long_lines, dplyr::filter(long_lines, FALSE))
  }

  expect_no_lines_long_lines('BEXSTA')
  expect_no_lines_long_lines('FOTEST')
  expect_no_lines_long_lines('BEXLTALL')
})

test_that("Source data variable names are preserved",{
  table_name <- "FOTEST"
  px1   <- temp_pxfile()
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

test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_erros <- function(table_name) {
    px_path <- temp_pxfile()

    pxmake_clean(get_metadata_path(table_name),
                 px_path,
                 get_source_data_path(table_name)
                 )

    output <- get_pxjobfile_path(table_name)

    pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
    expect_equal(0, pxjob_exit_code)

    # File is removed manually because pxjob_clean cannot be used because the
    # exit code from pxjob64Win::pxjob is needed.
    file.remove(output)
  }

  expect_that_pxjob_runs_without_erros("BEXLTALL")
  expect_that_pxjob_runs_without_erros("BEXSTA")
  expect_that_pxjob_runs_without_erros("FOTEST")
})

test_that("Value YES and NO are never quoted", {
  expect_no_lines_with_quoted_yes_no <- function(table_name) {
    px_path <- get_pxfile_path(table_name)

    pxmake_clean(get_metadata_path(table_name),
                 px_path,
                 get_source_data_path(table_name)
                 )

    px_lines <- readLines(px_path)

    invalid_lines <- px_lines[stringr::str_detect(px_lines, '=\"(YES|NO)\"')]

    expect_equal(invalid_lines, character(0))
  }

  expect_no_lines_with_quoted_yes_no("BEXLTALL")
  expect_no_lines_with_quoted_yes_no("BEXSTA")
  expect_no_lines_with_quoted_yes_no("FOTEST")
  expect_no_lines_with_quoted_yes_no("no_timeval_or_codes")
})
