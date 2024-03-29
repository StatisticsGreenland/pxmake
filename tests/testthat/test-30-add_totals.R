test_that("Total is added to one variable", {
  input <- tibble::tribble(~cohort, ~sex, ~value,
                               "a",  "m",     NA,
                               "a",  "f",      3,
                               "b",  "m",      2,
                               "b",  "f",      4
                           )

  expect <- tibble::tribble(~cohort, ~sex, ~value,
                                "a",  "m",     NA,
                                "a",  "f",      3,
                                "b",  "m",      2,
                                "b",  "f",      4,
                            "Total",  "m",      2,
                            "Total",  "f",      7
                            )

  expect_equal(add_total_level_to_var(input, 'cohort') %>% dplyr::arrange_all(),
               expect %>% dplyr::arrange_all())
})


test_that("Totals are added to two variables", {
  read_data <- function(table_name) {
    table_name %>%
      get_data_path() %>%
      readRDS %>%
      dplyr::as_tibble()
  }

  bexsta <- read_data("BEXSTA")
  bexsta_without_totals <- read_data("BEXSTA_WITHOUT_TOTALS")

  output <-
    add_totals_to_df(df = bexsta_without_totals,
                     variables = c("place of birth", "gender"),
                     level_name = "T",
                     sum_var = "persons"
                     ) %>%
    dplyr::arrange_all()

  expect_equal(dplyr::arrange_all(output), dplyr::arrange_all(bexsta))

  # Test having multiple level_names
  bexsta2 <-
    bexsta %>%
    dplyr::mutate(gender = ifelse(gender == "T", "Total", gender))

  output2 <-
    add_totals_to_df(df = bexsta_without_totals,
                     variables = c("place of birth", "gender"),
                     level_names = c("T", "Total"),
                     sum_var = "persons"
                     ) %>%
    dplyr::arrange_all()

  expect_equal(dplyr::arrange_all(output2), dplyr::arrange_all(bexsta2))
})


test_that("adds total levels to data without them", {
  metadata_path <- get_metadata_path("BEXSTA")
  px1           <- temp_px_file()
  px2           <- temp_px_file()

  px(input = metadata_path, data = get_data_path("BEXSTA")) %>%
    pxsave(path = px1)

  px(input = metadata_path, data = get_data_path("BEXSTA_WITHOUT_TOTALS")) %>%
    add_totals(c("place of birth", "gender")) %>%
    pxsave(path = px2)

  expect_equal_lines(px1, px2)
})
