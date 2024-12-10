# ERRORS
test_that("Only paths OR name/domain/df/ can be defined", {
  expect_error(px_classification(name = "name", vs_path = "value_set.vs"),
               regexp = 'path arguments'
               )

  expect_error(px_classification(prestext = "name",
                                 agg_paths = "aggregation.agg"
                                 ),
               regexp = 'path arguments'
               )
})

test_that(".vs and .agg files exists", {
  expect_error(px_classification(vs_path = "not_a_valueset.vs",
                                 agg_paths = agg_25years_path()
                                ),
               regexp = 'does not exist'
               )

  expect_error(px_classification(vs_path = vs_age5_path(),
                                 agg_paths = "not_an_aggregation.agg"
                                 ),
               regexp = 'does not exist'
               )
})

test_that("if agg_paths is defined vs_path should also", {
  expect_error(px_classification(agg_paths = agg_25years_path()),
               regexp = "'vs_path' is missing"
               )
})

test_that('if any non-path argument is defined, all must be defined', {
  expect_error(px_classification(name = "name",
                                 prestext = "prestext",
                                 domain = "domain"
                                 ),
               regexp = "'df' is missing"
               )

  expect_error(px_classification(name = "name",
                                 prestext = "prestext",
                                 df = age_classification
                                 ),
               regexp = "'domain' is missing"
               )

  expect_error(px_classification(name = "name",
                                 domain = "domain",
                                 df = age_classification
                                 ),
               regexp = "'prestext' is missing"
               )

  expect_error(px_classification(prestext = "prestext",
                                 domain = "domain",
                                 df = age_classification
                                 ),
               regexp = "'name' is missing"
               )
})

test_that("df has the right columns", {
  expect_error(px_classification(women,
                                 name = "name",
                                 prestext = "prestext",
                                 domain = "domain",
                                 regexp = "missing column 'valuecode'"
                                 )
               )
})

# WARNINGS

# agg paths in the vs file doesn't exists

# Heading is missing is vs/agg file. Maybe an error? Error for some headings
