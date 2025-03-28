test_that("px_classification_save saves .vs and .agg files", {
  tmp_dir <- tempdir()

  px_classification(name = "Age",
                    prestext = "Age table",
                    domain = "age",
                    df = age_classification
                    ) %>%
    px_save_classification(path = tmp_dir)

  expect_equal(list.files(tmp_dir, pattern = "(agg|vs)$"),
               c("10-years_classes.agg", "25-years_classes.agg", "Age.vs")
               )
})

test_that("NA values are not saved in .agg", {
  c <- px_classification(vs_path = vs_pxvsbrche_path())

  tmp_dir <- tempdir()

  px_save_classification(c, path = tmp_dir)

  agg_files <- list.files(tmp_dir, pattern = "\\.agg$", full.names = TRUE)

  for (agg_file in agg_files) {
    expect_false(any(readLines(agg_file) == "[NA]"))
  }
})
