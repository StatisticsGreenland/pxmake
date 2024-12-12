age_classification <-
  readr::read_tsv(file.path('data-raw', 'age_classification.tsv'),
                  na = "NA",
                  col_types = "ccff"
                  ) |>
  dplyr::mutate(across(c(-valuecode, -valuetext), ordered))

usethis::use_data(age_classification, overwrite = TRUE)
