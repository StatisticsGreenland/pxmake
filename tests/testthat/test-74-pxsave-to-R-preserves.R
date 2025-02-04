# Add test that running px_save('.R') code creates the same px object.
test_that('px file is preserved', {

  save_as_r_and_open <- function(x) {
    tmp_file <- temp_r_file()

    px_save(x, tmp_file)

    x2 <- source(tmp_file)$value

    return(x2)
  }

  expect_equal_all_px_element <- function(x) {
    x2 <- save_as_r_and_open(x)

    expect_equal(x$languages, x2$languages)
    expect_equal(x$table1, x2$table1)
    expect_equal(dplyr::arrange_all(x$table2), dplyr::arrange_all(x2$table2))
    expect_equal(x$variables1, x2$variables1)
    expect_equal(x$variables2, x2$variables2)
    expect_equal(x$cells1, x2$cells1)
    expect_equal(x$cells2, x2$cells2)
    expect_equal(x$acrosscells, x2$acrosscells)
    expect_equal(x$data, x2$data)
    expect_equal(x, x2)
  }

  expect_equal_all_px_element(px(population_gl))
  expect_equal_all_px_element(px(get_px_file_path('BEXSTA_windows_1252')))
  expect_equal_all_px_element(px(get_px_file_path('SOXATI4')))

  # UNITS in wrong, it's set for values that do not exist
  # expect_equal_all_px_element(px(get_px_file_path('CONTVARIABLE')))

  # expect_equal_all_px_element(px(get_px_file_path('CONTVARIABLE_multiple_languages')))
  # expect_equal_all_px_element(px(get_px_file_path('no_timeval_or_codes2')))
  # expect_equal_all_px_element(px(get_px_file_path('PRXPRISH')))

#  expect_equal_all_px_element(px(get_px_file_path('TUX01')))
})
