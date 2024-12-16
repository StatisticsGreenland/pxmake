test_that("Error if vs_file is missing heading", {
  expect_error(px_classification_from_path(vs_path =
                                             get_classification_path("vs_missing_mandatory_section.vs")
                                           ),
               regexp = "missing mandatory section"
               )
})
