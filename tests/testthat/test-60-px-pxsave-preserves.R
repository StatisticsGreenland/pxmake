test_that("px file = px_save(px(px file))", {
  expect_px_px_save_preserves_everything(px_from_table_name("BEXSTA"))
  expect_px_px_save_preserves_everything(px_from_table_name("FOTEST"))
  expect_px_px_save_preserves_everything(px_from_table_name("no_timeval_or_codes"))
  expect_px_px_save_preserves_everything(px_from_table_name("zero_heading"))
  expect_px_px_save_preserves_everything(px_from_table_name("zero_stub"))


  # Cell values with closing parenthesis )
  population_gl |>
    dplyr::mutate(across(age, ~ dplyr::case_when(. == "0-6"  ~ "(0-6)",
                                                 . == "7-16"  ~ "(7)-16",
                                                 . == "17-24" ~ "17-(24)",
                                                 . == "25-64" ~ "25)-64",
                                                 . =="65+"    ~ "65(",
                                                 TRUE ~ .
                                                 )
                         )
                  ) %>%
    px() %>%
    px_precision(dplyr::tribble(~`variable-code`, ~precision,
                                'age', 3)
                 ) %>%
    px_cellnote(
      dplyr::tribble(~gender,   ~age,  ~year, ~cellnote,
                     'female',    '*', '2014', 'Uncertainty in ages',
                     NA, "(0-6)", NA, 'Note to value with parensthsis ()'
                     )) %>%
    expect_px_px_save_preserves_everything()
})
