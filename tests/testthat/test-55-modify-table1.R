test_that('Table1 is modified', {
  x <-
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

  expect_equal(dplyr::arrange(dplyr::filter(x$table1, keyword %in% target$keyword),
                              keyword
                              ),
               dplyr::arrange(target, keyword)
               )
})
