test_that("Error if vs_file is missing heading", {
  expect_error(px_classification_from_path(vs_path =
                                             get_classification_path("vs_missing_mandatory_section.vs")
                                           ),
               regexp = "missing mandatory section"
               )
})

test_that("Warning if length of [Valuecode] and [Valuetext] differ", {
  expect_warning(px_classification_from_path(vs_path =
                                               get_classification_path("vs_different_lengths1.vs")
                                             ),
                 regexp = "different number of rows"
                 )

  expect_warning(px_classification_from_path(vs_path =
                                               get_classification_path("vs_different_lengths2.vs")
                                             ),
                 regexp = "different number of rows"
                 )
})
