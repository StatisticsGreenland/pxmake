project_dir <- file.path('..', '..')
source(file.path(project_dir, 'R', 'helper_functions.R'))

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
