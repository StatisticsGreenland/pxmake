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
    px_save(path = px1)

  px(input = metadata_path, data = get_data_path("BEXSTA_WITHOUT_TOTALS")) %>%
    px_add_totals(c("place of birth", "gender")) %>%
    px_save(path = px2)

  expect_equal_lines(px1, px2)
})

test_that("metadata without elimination values use default value", {
  levels <-
    px(input = get_metadata_path("BEXSTA"),
       data = get_data_path("BEXSTA_WITHOUT_TOTALS")
       ) %>%
    px_elimination(NULL) %>%
    px_add_totals(value = "gender") %>%
    magrittr::extract2("data") %>%
    dplyr::pull(gender) %>%
    unique()

  expect_equal(levels, c("Total", "K", "M"))
})

test_that("do not ignore NA when summing", {
  df <- tibble::tribble(~category, ~n,
                        "a",  10,
                        "b",  20,
                        "c",  NA
                        )

  result <-
    px(df) %>%
    px_add_totals("category", na.rm = FALSE) %>%
    magrittr::extract2("data") %>%
    dplyr::pull(n)

  expect_identical(result, c(NA, 10, 20, NA))
})

test_that("Add totals work for multiple languages", {
  # Create px object with multilingual add_totals
  x <-
    population_gl %>%
    px() %>%
    px_languages(c('en', 'da')) %>%
    px_elimination(dplyr::tribble(~`variable-code`, ~elimination,
                                  'gender', 'Total'
                                  )
                   ) %>%
    px_add_totals('gender')

  # px_add_totals adds both 'en' and 'da' level in data. This is a mistake.
  gender_levels <-
    x %>%
    px_data() %>%
    dplyr::distinct(gender) %>%
    dplyr::pull(gender)

  expect_identical(gender_levels, c("Total", "male", "female"))
})

test_that("Add totals adds elimination", {
  default_elimination <-
    population_gl %>%
    px() %>%
    px_add_totals('gender') %>%
    px_elimination()

  expect_identical(default_elimination,
                   dplyr::tibble('variable-code' = 'gender',
                                 'elimination' = 'Total'
                                 )
                   )
})

test_that("Elimination and values are preserved when changing data frame", {
  tmp_px_file <- temp_px_file()

  population_gl %>%
    px() %>%
    px_add_totals('gender') %>%
    px_save(tmp_px_file)

  x <-
    px(tmp_px_file) %>%
    px_data(population_gl)

  expect_identical(px_elimination(x),
                   dplyr::tibble('variable-code' = 'gender',
                                 'elimination' = 'Total'
                                 )
                   )

  elimination_values <-
    x %>%
    px_values() %>%
    dplyr::semi_join(px_elimination(x),
                     by = c('variable-code' = 'variable-code',
                            'code' = 'elimination'
                            )
                     ) %>%
    dplyr::select("variable-code", "elimination" = "code")

  expect_identical(elimination_values, px_elimination(x))
})
