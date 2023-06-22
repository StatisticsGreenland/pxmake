test_that("px file is valid", {
  # Create 4 px files that all tests are run on
  bexsta              <- create_px_file("BEXSTA")
  bexltall            <- create_px_file("BEXLTALL")
  fotest              <- create_px_file("FOTEST")
  no_timeval_or_codes <- create_px_file("no_timeval_or_codes")

  test_that("px lines matches regexp", {
    keywords <- get_px_keywords() %>% dplyr::pull(keyword)

    valid_lines <-
      c(paste0("^", keywords, "[=\\[\\(]"), # keyword followed by [ ( or =
        '^".+[",;]$',                       # continued lines (when previous is longer than 256)
        '^[e[:digit:][:space:]"-.]+$',      # data lines
        '^;$'                               # last line of file
      )

    regex <- paste0(valid_lines, collapse = "|")

    expect_no_invalid_lines <- function(path) {
      px_lines <- readLines(path)

      invalid_lines <- px_lines[stringr::str_detect(px_lines, regex, negate = TRUE)]

      expect_equal(invalid_lines, character(0))
    }

    expect_no_invalid_lines(bexsta)
    expect_no_invalid_lines(bexltall)
    expect_no_invalid_lines(fotest)
    expect_no_invalid_lines(no_timeval_or_codes)
  })

  test_that("Codes are defined for all values", {
    expect_code_and_values_match <- function(path) {
      px_lines <- readLines(path)

      value_lines <- px_lines[stringr::str_detect(px_lines, '^VALUES')]
      code_lines  <- px_lines[stringr::str_detect(px_lines, '^CODES')]

      # Equal number of variables
      expect_equal(length(value_lines), length(code_lines))

      # Equal number of values for each variable
      expect_equal(stringr::str_count(value_lines, '","'),
                   stringr::str_count(code_lines,  '","')
                   )
    }

    expect_code_and_values_match(bexsta)
    expect_code_and_values_match(bexltall)
    expect_code_and_values_match(fotest)
    expect_code_and_values_match(no_timeval_or_codes)
  })

  test_that("header lines doesn't exceed 256 characters", {
    expect_no_too_long_lines <- function(path) {
      px_lines <- readLines(path)

      data_line_index <- stringr::str_which(px_lines, '^DATA=$')

      long_lines <-
        tibble::tibble(line = px_lines[1:data_line_index]) %>%
        dplyr::mutate(text = stringr::str_extract(line, "(?<==).+"),
                      length = nchar(text)
                      ) %>%
        dplyr::filter(length > 256)

      expect_equal(long_lines, dplyr::filter(long_lines, FALSE))
    }

    expect_no_too_long_lines(bexsta)
    expect_no_too_long_lines(bexltall)
    expect_no_too_long_lines(fotest)
    expect_no_too_long_lines(no_timeval_or_codes)
  })

  test_that("Value YES and NO are never quoted", {
    expect_no_lines_with_quoted_yes_no <- function(path) {
      px_lines <- readLines(path)

      invalid_lines <- px_lines[stringr::str_detect(px_lines, '=\"(YES|NO)\"')]

      expect_equal(invalid_lines, character(0))
    }

    expect_no_lines_with_quoted_yes_no(bexsta)
    expect_no_lines_with_quoted_yes_no(bexltall)
    expect_no_lines_with_quoted_yes_no(fotest)
    expect_no_lines_with_quoted_yes_no(no_timeval_or_codes)
  })

  test_that("timevals are added", {
    expect_that_px_filehas_timeval <- function(path) {
      px_lines <- readLines(path)

      timeval_lines <- px_lines[stringr::str_detect(px_lines, "^TIMEVAL")]

      expect_true(length(timeval_lines) > 0)
    }

    expect_that_px_filehas_timeval(bexsta)
    expect_that_px_filehas_timeval(bexltall)
    expect_that_px_filehas_timeval(fotest)
  })

  expect_true(TRUE) #needed to run
})
