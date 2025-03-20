test_that('CONTVARIABLE is handled correctly', {
  x <- px(get_px_file_path("CONTVARIABLE"))

  contvariable_name <- "ContentsCode"

  # CONTVARIABLE should be TRUE
  x$variables1 %>%
    dplyr::filter(`variable-code` == contvariable_name) %>%
    dplyr::pull(contvariable) %>%
    expect_true()

  # CONTVARIABLE should be FALSE for all other variables
  x$variables1 %>%
    dplyr::filter(`variable-code` != contvariable_name) %>%
    dplyr::pull(contvariable) %>%
    any() %>%
    expect_false()

  # CONTVARIABLE should not be created as a keyword in table2
  x$table2 %>%
    dplyr::filter(keyword == "CONTVARIABLE") %>%
    nrow() %>%
    expect_equal(0)
})
