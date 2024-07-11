#' This test primarily exist to increase the test coverage

test_that("Documentation returns strings", {
  expect_string <- function(x) {
    expect_true(is.character(x))
  }

  expect_identical(keyword_to_function('AXIS-VERSION'), 'px_axis_version')
  expect_identical(function_to_keyword('px_axis_version'), 'AXIS-VERSION')
  expect_identical(split_multiline_str_into_vector("a\nb"), c('a', 'b'))
  expect_string(add_documentation_table1("MATRIX", "B"))
  expect_string(add_documentation_table1("DECIMALS", "B"))
  expect_string(add_documentation_table2("A", "B", "C"))
  expect_string(add_documentation_head_stub("A"))
  expect_string(add_documentation_variables2("A", "B", "C", "D"))
  expect_string(cells1_example("A", "B", "C"))
  expect_string(cells2_example("A", "B", "C", "D"))
  expect_string(note_description("A"))
  expect_string(return_px_or_df())
  expect_string(doc_keyword_function_intro("ORDER"))
  expect_string(description_start("CONTENTS"))
  expect_string(table_param_value_ending("CONTENTS"))
  expect_string(table_param_value_ending("DESCRIPTION"))
  expect_string(table1_param_value("CONTENTS"))
  expect_string(table2_param_value("CONTENTS"))
  expect_string(cells_param_value("CONTENTS", "1"))
  expect_string(cells_param_value("CONTENTS", "2"))
  expect_string(variables2_param_value("CONTENTS"))
  expect_string(acrosscells_param_value("CONTENTS"))
  expect_string(note_param_value("CONTENTS"))
  expect_string(pivot_param_value("CONTENTS"))
})
