test_that("error - invalid px object", {
  expect_error(validate_px(1),
               regexp = "px object must be a list"
  )

  expect_error(validate_px(list()),
               regexp = "px object is missing these names:"
               )

  expect_error(validate_px(c(get_base_px(), "extra"=NA)),
               regexp = "px object contains invalid names:"
               )

  px1 <- get_base_px()
  px1$languages <- c("en")

  expect_error(validate_px(px1),
               regexp = "px object element 'languages' must be a data frame"
               )

  px2 <- get_base_px()
  px2$table1 <- data.frame(Keyword = as.character())

  expect_error(validate_px(px2),
               regexp = "px object is missing these names in element 'table1':"
               )


})

test_that("valid px object", {
  expect_valid_px <- function(x) {
    expect_equal(x, validate_px(x))
  }

  expect_valid_px(list(languages = get_base_languages(),
                       table1 = get_base_table1(),
                       table2 = get_base_table2(),
                       variables1 = get_base_variables1(),
                       variables2 = get_base_variables2(),
                       codelists1 = get_base_codelists1(),
                       codelists2 = get_base_codelists2(),
                       data = get_base_data()
                       )
                  )
})
