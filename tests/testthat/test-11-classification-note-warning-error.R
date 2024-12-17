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

test_that(".vs files exists", {
  expect_error(px_classification(vs_path = "not_a_valueset.vs",
                                 agg_paths = agg_25years_path()
                                ),
               regexp = 'does not exist'
               )
})

test_that(".agg files exists", {
  expect_warning(px_classification(vs_path = vs_age5_path(),
                                   agg_paths = "not_an_aggregation.agg"
                                   ),
                 regexp = 'do not exist'
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
               regexp = '"df" is missing'
               )

  expect_error(px_classification(name = "name",
                                 prestext = "prestext",
                                 df = age_classification
                                 ),
               regexp = '"domain" is missing'
               )

  expect_error(px_classification(name = "name",
                                 domain = "domain",
                                 df = age_classification
                                 ),
               regexp = '"prestext" is missing'
               )

  expect_error(px_classification(prestext = "prestext",
                                 domain = "domain",
                                 df = age_classification
                                 ),
               regexp = '"name" is missing'
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

test_that("Error if wrong input formats", {
  expect_error(px_classification(name = c("name", "name2"),
                                 prestext = "prestext",
                                 domain = "domain",
                                 df = data.frame(valuecode = c(1, 2, 3),
                                                 valuetext = c("a", "b", "c")
                                                 )
                                 ),
               regexp = "'name' must be a character string"
               )

  expect_error(px_classification(name = "name",
                                 prestext = 5,
                                 domain = "domain",
                                 df = data.frame(valuecode = c(1, 2, 3),
                                                 valuetext = c("a", "b", "c")
                                                 )
                                 ),
               regexp = "'prestext' must be a character string"
               )

  expect_error(px_classification(name = "name",
                                 prestext = "prestext",
                                 domain = data.frame(),
                                 df = data.frame(valuecode = c(1, 2, 3),
                                                 valuetext = c("a", "b", "c")
                                                 )
                                 ),
               regexp = "'domain' must be a character string"
               )

  expect_error(px_classification(name = "name",
                                 prestext = "prestext",
                                 domain = "domani",
                                 df = c("valuetext")
                                 ),
               regexp = "'df' must be a data frame"
               )

  expect_error(px_classification(name = "name",
                                 prestext = "prestext",
                                 domain = "domain",
                                 df = data.frame(no_valuecode = c(1, 2, 3),
                                                 valuetext = c("a", "b", "c")
                                                 )
                                 ),
               regexp = "missing column 'valuecode'"
               )
})

# ERROR
# If trying to read a non-option (Aggtext, Valuetext) heading?
# E.g. when an agg file has a heading that doesn't match a group

# WARNINGS
# valuetext and valuecode have differing lengths
# A chunk contains invalid formatted lines. All should be digit|Name|Pretext=value

