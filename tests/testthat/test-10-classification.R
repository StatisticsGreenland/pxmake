test_that("A classification can be created from df and paths", {

  c1 <- px_classification(name = "Test",
                          domain = "Test",
                          df = data.frame(code = 1:26, text = letters)
                          )

  expect_s3_class(c1, "classification")

  c2 <- px_classification(vs_path = vs_age5_path(),
                          agg_paths = agg_25years_path()
                          )

  expect_s3_class(c2, "classification")
})

test_that("Classification can be created without aggregation", {
  c1 <- px_classification(vs_path = vs_age5_path(),
                          agg_paths = character(0)
                          )

  expect_s3_class(c1, "classification")

  c2 <- px_classification(name = "No aggregation",
                          domain = "no_agg",
                          df = dplyr::tribble(~valuecode,    ~valuetext,
                                                   "0-4",   "0-4 years",
                                                   "5-9",   "5-9 years",
                                                 "10-14", "10-14 years"
                                              )
                          )

  expect_s3_class(c2, "classification")
})

test_that("Classification automatically read agg files", {
  c <- px_classification(vs_path = vs_age5_path())

  expect_equal(c$df, age_classification)
})
