#' This test primarily exist to increase the test coverage

test_that("Documentation returns strings", {
  expect_string <- function(x) {
    expect_true(is.character(x))
  }

  expect_string(description_start("CONTENTS"))
  expect_string(table_description("CONTENTS"))
  expect_string(table_param_value_ending("CONTENTS"))
  expect_string(table_param_value_ending("DESCRIPTION"))
  expect_string(table1_param_value("CONTENTS"))
  expect_string(table2_param_value("CONTENTS"))
  expect_string(cells_param_value("CONTENTS", "1"))
  expect_string(cells_param_value("CONTENTS", "2"))
  expect_string(variables2_param_value("CONTENTS"))
  expect_string(acrosscells_param_value("CONTENTS"))
  expect_string(note_param_value("CONTENTS"))
  expect_string(pivot_param_variables("CONTENTS"))
})
