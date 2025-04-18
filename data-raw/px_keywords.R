library(readr)

px_keywords <-
  readr::read_tsv(file.path('data-raw', 'keywords.tsv'),
                  na = "NA",
                  col_types = "clllllcc"
                  ) %>%
  dplyr::mutate(order = dplyr::row_number(),
                px_function = keyword_to_function(keyword)
                ) %>%
  dplyr::relocate(px_function, .after = "keyword")

usethis::use_data(px_keywords, overwrite = TRUE)
