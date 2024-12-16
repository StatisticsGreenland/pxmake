test_that("Aggregation df is created", {
  expect_identical(aggregation_df(agg_10years_path()),
                   dplyr::select(age_classification, valuecode, `10-years_classes`)
                   )

  expect_identical(aggregation_df(agg_25years_path()),
                   dplyr::select(age_classification, valuecode, `25-years_classes`)
                   )
})


test_that("Warning, aggreation groups and 'Aggtext' differ in length", {
  expect_warning(aggregation_df(get_classification_path("agg_different_lengths1.agg")),
                 regexp = "number of aggregation groups.*differ"
                 )

  expect_warning(aggregation_df(get_classification_path("agg_different_lengths2.agg")),
                 regexp = "number of aggregation groups.*differ"
                 )
})

test_that("Warning, if aggregation group is missing", {
  expect_warning(aggregation_df(get_classification_path("agg_missing_group.agg")),
                 regexp = "No group with label"
                 )
})

test_that("Error, if missing mandatory section", {
  expect_error(aggregation_df(get_classification_path("agg_missing_mandatory_section.agg")),
               regexp = "missing mandatory section")

})
