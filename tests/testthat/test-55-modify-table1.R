test_that('Table1 is modified', {
  p <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px() %>%
    charset("ANSI") %>%
    creation_date("2019-01-01 10:00") %>%
    matrix("BEXSTA") %>%
    decimals("1")


  target <-
    tibble::tribble(
              ~keyword, ~value,
             "CHARSET", "ANSI",
        "AXIS-VERSION",     NA,
            "CODEPAGE",     NA,
            "LANGUAGE",     NA,
       "CREATION-DATE", "2019-01-01 10:00",
         "NEXT-UPDATE",     NA,
    "UPDATE-FREQUENCY",     NA,
             "TABLEID",     NA,
            "DECIMALS",     "1",
        "SHOWDECIMALS",     NA,
              "MATRIX", "BEXSTA",
       "AGGREGALLOWED",     NA,
             "AUTOPEN",     NA,
        "SUBJECT-CODE",     NA,
        "CONFIDENTIAL",     NA,
           "COPYRIGHT",     NA
    ) %>%
    tidyr::drop_na(value)

  expect_equal(dplyr::arrange(dplyr::filter(p$table1, keyword %in% target$keyword),
                              keyword
                              ),
               dplyr::arrange(target, keyword)
               )
})


# Table1 keywords
# keyword
# <chr>
# 1 CHARSET
# 2 AXIS-VERSION
# 3 CODEPAGE
# 4 LANGUAGE
# 5 CREATION-DATE
# 6 NEXT-UPDATE
# 7 UPDATE-FREQUENCY
# 8 TABLEID
# 9 DECIMALS
# 10 SHOWDECIMALS
# 11 MATRIX
# 12 AGGREGALLOWED
# 13 AUTOPEN
# 14 SUBJECT-CODE
# 15 CONFIDENTIAL
# 16 COPYRIGHT
