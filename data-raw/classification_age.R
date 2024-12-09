age_classification <-
  readr::read_tsv(file.path('data-raw', 'classification_age.tsv'),
                  na = "NA",
                  col_types = "ccff"
                  ) |>
  dplyr::mutate(across(c(-valuecode, -valuetext), ordered))

usethis::use_data(age_classification, overwrite = TRUE)
