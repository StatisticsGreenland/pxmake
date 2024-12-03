test_that("px_classification_save saves .vs and .agg files", {
  tmp_dir <- tempdir()

  px_classification(name = "Age",
                    domain = "age",
                    df = age_classification
                    ) %>%
    px_save_classification(directory = tmp_dir)

  expect_equal(list.files(tmp_dir, pattern = "(agg|vs)$"),
               c("10 years classes.agg", "25 years classes.agg", "Age.vs")
               )
})
