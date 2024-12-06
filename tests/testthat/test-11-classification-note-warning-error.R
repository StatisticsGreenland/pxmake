# vs_path should be defined but agg_paths can be empty

# check that .vs and .agg has proper format

test_that("Only paths OR name/domain/df/ can be defined", {
  expect_error(px_classification(name = "name", vs_path = "value_set.vs"),
               regexp = 'arguments'
               )

  expect_error(px_classification(prestext = "name",
                                 agg_paths = "aggregation.agg"
                                 ),
               regexp = 'arguments'
               )
})

test_that(".vs and .agg files exists", {
  expect_error(px_classification(vs_path = "not_a_valueset.vs",
                                 agg_paths = agg_25years_path()
                                ),
               regexp = 'file not exists'
               )

  expect_error(px_classification(vs_path = vs_age5_path(),
                                 agg_paths = "not_an_aggregation.agg"
                                 ),
               regexp = 'file not exists'
               )
})

test_that("if agg_paths is defined vs_path should also", {
  expect_error(px_classification(agg_paths = agg_25years_path()),
               regexp = 'vs_path'
               )
})


test_that("df has the right columns", {
  expect_error(women, regexp = 'df is missing column valuecode')

})

test_that("df columns have right format", {
  expect_error(px_classification(name = "test",
                                 prestext = "test",
                                 domain = "test",
                                 df = tibble::tibble(valuecode = 1:10,
                                                     valuetext = letters[1:10]
                                                     )
                                 ),
               regexp = 'character'
               )
})


# Check that agg paths in the vs file actually exist before reading them
