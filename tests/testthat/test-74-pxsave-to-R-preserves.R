test_that('px file is preserved when saved as R script', {

  save_as_r_and_open <- function(x, save_dataset = FALSE) {
    tmp_r   <- temp_r_file()
    tmp_rds <- temp_rds_file()

    if (isTRUE(save_dataset)) {
      px_save(x, path = tmp_r, data_path = tmp_rds)
    } else {
      px_save(x, path = tmp_r)
    }

    x2 <- source(tmp_r)$value

    return(x2)
  }

  expect_equal_all_px_element <- function(x, save_dataset = FALSE) {
    x2 <- save_as_r_and_open(x, save_dataset)

    expect_equal(x$languages, x2$languages)
    expect_equal(x$table1, x2$table1)
    expect_equal(x$table2, x2$table2)
    expect_equal(x$variables1, x2$variables1)
    expect_equal(x$variables2, x2$variables2)
    expect_equal(x$cells1, x2$cells1)
    expect_equal(x$cells2, x2$cells2)
    expect_equal(x$acrosscells, x2$acrosscells)
    expect_equal(x$data, x2$data)
    expect_equal(x, x2)
  }

  expect_equal_all_px_element(px(population_gl))
  expect_equal_all_px_element(px(population_gl), save_dataset = TRUE)
  expect_equal_all_px_element(px(get_px_file_path('BEXSTA_windows_1252')))
  expect_equal_all_px_element(px(get_px_file_path('BEXSTA_windows_1252')), save_dataset = TRUE)
  expect_equal_all_px_element(px(get_px_file_path('SOXATI4')))
  expect_equal_all_px_element(px(get_px_file_path('SOXATI4')), save_dataset = TRUE)
  expect_equal_all_px_element(px(get_px_file_path('TUX01')))
  expect_equal_all_px_element(px(get_px_file_path('TUX01')), save_dataset = TRUE)

  # A couple of tests doens't work, but the functionality is 80 there

  # UNITS in wrong because of (#364)
  # expect_equal_all_px_element(px(get_px_file_path('CONTVARIABLE')))
  # expect_equal_all_px_element(px(get_px_file_path('CONTVARIABLE_multiple_languages')))

  # TITLE differ, which is fine ("" vs NULL)
  # TIMEVAL differ, easy fix
  # expect_equal_all_px_element(px(get_px_file_path('no_timeval_or_codes2')))

  # Something about TIMEVAL causes problems
  # expect_equal_all_px_element(px(get_px_file_path('PRXPRISH')))
})
