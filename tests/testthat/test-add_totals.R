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
  read_source_data <- function(table_name) {
    table_name %>%
      get_source_data_path() %>%
      readRDS %>%
      dplyr::as_tibble()
  }

  bexsta <- read_source_data("BEXSTA")
  bexsta_without_totals <- read_source_data("BEXSTA_WITHOUT_TOTALS")

  output <-
    add_totals(df = bexsta_without_totals,
               vars = c("place of birth", "gender"),
               level_name = "T",
               sum_var = "value"
               ) %>%
    dplyr::arrange_all()

  expect_equal(dplyr::arrange_all(output), dplyr::arrange_all(bexsta))

  # Test having multiple level_names
  bexsta2 <-
    bexsta %>%
    dplyr::mutate(gender = ifelse(gender == "T", "Total", gender))

  output2 <-
    add_totals(df = bexsta_without_totals,
               vars = c("place of birth", "gender"),
               level_names = c("T", "Total"),
               sum_var = "value"
               ) %>%
    dplyr::arrange_all()

  expect_equal(dplyr::arrange_all(output2), dplyr::arrange_all(bexsta2))
})


test_that("pxmake adds total levels to data without them", {
  metadata_path <- get_metadata_path("BEXSTA")
  px_expect     <- tempfile()
  px_output     <- tempfile()

  pxmake_clean(metadata_path,
               px_expect,
               get_source_data_path("BEXSTA")
               )

  pxmake_clean(metadata_path,
               px_output,
               get_source_data_path("BEXSTA_WITHOUT_TOTALS"),
               add_totals = c("place of birth", "gender")
               )


  output <- readLines(px_output)
  expect <- readLines(px_expect)

  expect_equal(output, expect)
})
