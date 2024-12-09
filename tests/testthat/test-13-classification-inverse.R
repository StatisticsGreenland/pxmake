# Check that reading -> saving -> reading creates the same classification object

test_that("classification is preserved", {
  px_classification(name = "test",
                    prestext = "test",
                    domain = "test",
                    df = tibble::tibble(valuecode = as.character(1:10),
                                        valuetext = letters[1:10]
                                        )
                    ) %>%
    expect_save_read_preserves_classification()

  px_classification(vs_path = vs_age5_path(),
                    agg_paths = agg_25years_path()
                    ) %>%
    expect_save_read_preserves_classification()

  px_classification(vs_path = vs_age5_path()) %>%
    expect_save_read_preserves_classification()
})

