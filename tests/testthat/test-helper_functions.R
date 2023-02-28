test_that("strings are quoted", {
  input  <- c('hi',  ' " ',    'single\'quote')
  expect <- c('"hi"', '" " "', '"single\'quote"')

  expect_equal(str_quote(input), expect)
})

test_that("keywords has language", {
  expect_equal(add_language_to_keyword(keyword = rep('STUB', 4),
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
                                                "inunngorfik"
                                                )
                                      ),
               c('ELIMINATION("place of birth")',
                 'ELIMINATION[da]("fødested")',
                 'ELIMINATION[kl]("inunngorfik")'
                 )
               )
})

test_that("strings are quoted", {
  input  <- c(  'A',   ' b ',   '"', 0, 3.14, 'YES', 'NO', '"is quoted"')
  expect <- c('"A"', '" b "', '"""', 0, 3.14, 'YES', 'NO', '"is quoted"')

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


