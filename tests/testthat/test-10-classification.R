test_that("A classification can be created from df and paths", {
  c1 <- px_classification(name = "Test",
                          prestext = "Test",
                          domain = "Test",
                          df = data.frame(valuecode = 1:26, valuetext = letters)
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
                          prestext = "No aggregation",
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

test_that("Classification is robust to formatting differences in files", {
  target <- px_classification(vs_path = vs_age5_path())
  c      <- px_classification(vs_path = vs_age5_strangely_formatted_path())

  expect_identical(target$name, c$name)
  expect_identical(target$prestext, c$prestext)
  expect_identical(target$domain, c$domain)
  expect_identical(target$df$valuecode, c$df$valuecode)
  expect_identical(target$df$valuetext, c$df$valuetext)
  expect_identical(target$df[[3]], c$df[[3]])
  expect_identical(target$df[[4]], c$df[[4]])
})

test_that("Classification can be create without [Valuetext]", {
  expect_s3_class(
    px_classification(vs_path = get_classification_path("vs_without_valuetext.vs")),
    "classification"
    )
})
