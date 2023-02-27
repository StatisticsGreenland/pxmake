# Test pxmake.
#
# The 3 data tables asserts the following features:
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

test_that("pxmake runs without errors and creates a file", {
  test_file_creation <- function(table_name) {
    if (file.exists(get_pxfile_path(table_name))) {
      file.remove(get_pxfile_path(table_name))
    }

    if (table_name == "FOTEST") {
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
})

test_that("pxmake adds total levels to data without them", {
  px_output <- get_pxfile_path("BEXSTA_ADDED_TOTALS")

  pxmake(get_metadata_path("BEXSTA"),
         px_output,
         get_source_data_path("BEXSTA_WITHOUT_TOTALS"),
         add_totals = c("place of birth", "gender")
         )

  output <- readLines(px_output)
  expect <- readLines(get_pxfile_path("BEXSTA"))

  expect_equal(output, expect)

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
