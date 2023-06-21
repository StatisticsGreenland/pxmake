test_that("strings are quoted", {
  input  <- c('hi',  ' " ',    'single\'quote')
  expect <- c('"hi"', '" " "', '"single\'quote"')

  expect_equal(str_quote(input), expect)
})

test_that("keywords has language", {
  expect_equal(add_language_to_keyword(keyword = rep('STUB', 4),
                                       main_language = "en",
                                       language = c("en", "da", "kl", NA)
                                       ),
               c("STUB", "STUB[da]", "STUB[kl]", "STUB")
               )
})

test_that("keyword has sub-key", {
  expect_equal(add_sub_key_to_keyword(keyword = c("ELIMINATION",
                                                  "ELIMINATION[da]",
                                                  "ELIMINATION[kl]"),
                                      name    = c("place of birth",
                                                  "fødested",
                                                  NA)
                                      ),
               c('ELIMINATION("place of birth")',
                 'ELIMINATION[da]("fødested")',
                 'ELIMINATION[kl]'
                 )
               )
})

test_that("keyword has cell name", {
  expect_equal(add_cell_to_keyword(keyword = c('PRECISION("measure")',
                                               'PRECISION[da]("measure")'
                                               ),
                                   name = c("Life expectancy", NA)
                                   ),
               c('PRECISION("measure","Life expectancy")',
                 'PRECISION[da]("measure")')
               )
})

test_that("strings are quoted", {
  input  <- c(  'A',   ' b ',   '"', 0, 3.14, 'YES', 'NO', '"is quoted"', 'TLIST(A1,"2015";')
  expect <- c('"A"', '" b "', '"""', 0, 3.14, 'YES', 'NO', '"is quoted"', 'TLIST(A1,"2015";')

  expect_equal(quote_unless_numeric_or_yes_no(input), expect)
})

test_that("list values are unique and sorted", {
  lst <- list(y = c(2, 1, 3, 3, 3), x = 'a', z = c(rep('b', 4), 'a'))

  expect <- list(x = 'a', y = c(1, 2, 3), z = c('a', 'b'))

  expect_equal(lst_distinct_and_arrange(lst), expect)
})

test_that("lists are merged and sorted", {
  lst1 <- list(x = 1:2, y = 'a')
  lst2 <- list(x = 2:3, y = c('b', 'c'))

  expect <- list(x = c(1, 2, 3), y = c('a', 'b', 'c'))

  expect_equal(merge_named_lists(lst1, lst2), expect)
})

test_that("Time values are classified", {
  test_equal <- function(values, expect) {
    expect_equal(get_timeval_type_from_values(values), expect)
  }

  test_equal(c("1995", "1996", "1997")         , "A")
  test_equal(c("1995H1", "1995H2", "1996H1")   , "H")
  test_equal(c("1995Q1", "1995Q2", "1995Q3")   , "Q")
  test_equal(c("1995M01", "1995M02", "1995M03"), "M")
  test_equal(c("1995W01", "1995W02", "1005W03"), "W")

  test_equal(c("", NA, NULL, "2001Q1")         , "Q")
})

test_that("Vectors are zipped", {
  expect_equal(zip_vectors(c(1, 3, 5), c(2, 4, 6)),
               c(1, 2, 3, 4, 5, 6)
               )
})

test_that("long lines are split into <=256 character bits", {
  expect_equal(break_long_lines(""), "")
  expect_equal(break_long_lines(NA), "")
  expect_equal(break_long_lines(NULL), "")

  input1  <- '"a", "b", "c"'
  expect1 <- '"a", "b", "c"'
  expect_equal(break_long_lines(input1), expect1)

  str_px_values_vector <- function(str) {
    str_quote(paste0(str, collapse = '","'))
  }

  input2 <- str_px_values_vector(1:104)
  expect2 <- c(paste0(str_px_values_vector(1:53), ","),
               paste0(str_px_values_vector(54:103), ","),
               str_px_values_vector(104)
               )
  expect_equal(break_long_lines(input2), expect2)

  input3  <- '"string which is a single value"'
  expect3 <- c('"string which is a"',
               '" single value"'
               )
  expect_equal(break_long_lines(input3, max_line_length = 20), expect3)

  input4 <- paste0('"a long string","composed of multiple values",',
                   '"some of which are longer than the line limit of 50 characters",',
                   '"and here are","a few", "short","values"'
                   )

  expect4 <- c('"a long string","composed of multiple values",',
               '"some of which are longer than the line limit of"',
               '" 50 characters","and here are","a few", "short",',
               '"values"'
               )
  expect_equal(break_long_lines(input4, max_line_length = 50), expect4)
})

test_that("Variables are converted to lists", {
  input <- tidyr::tibble(x = letters[1:3],
                         y = c(1, 2, NA)
                         )

  expect <- tidyr::tibble(x = letters[1:3],
                          y = c(list(1), list(2), list(NA_real_))
                          )

  expect_equal(wrap_varaible_in_list(input, y), expect)
})

test_that("File extensions work", {
  expect_true(is_xlsx_file('test.xlsx'))
  expect_true(is_rds_file('test.rds'))
  expect_true(is_px_file('test.PX'))
  expect_false(is_xlsx_file('test.px'))
  expect_false(is_xlsx_file(NULL))
  expect_false(is_xlsx_file(TRUE))
  expect_false(is_xlsx_file(data.frame()))
})

test_that("file encoding is correct", {
  get_file_encoding_for_table <- function(table_name) {
    get_encoding_from_px_file(get_pxfile_path(table_name))
  }

  expect_equal(get_file_encoding_for_table('TUX01'),   'iso-8859-15')
  expect_equal(get_file_encoding_for_table('BEXSTA_windows_1252'), 'Windows-1252')

  # no encoding listed; utf-8 is default
  px_file <- temp_pxfile()
  pxmake_clean(get_metadata_path("BEXSTA"),
               px_file,
               get_source_data_path("BEXSTA")
  )
  expect_equal(get_encoding_from_px_file(px_file),  'utf-8')
})