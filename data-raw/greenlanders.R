library(dplyr)

draw <- function(values) {
  sample(values, size = 100, replace = TRUE)
}

greenlanders <-
  tibble(cohort       = draw(c("A", "B")),
         gender       = draw(c("male", "female")),
         age          = draw(18:99),
         municipality = draw(c("Avannaata",
                               "Kujalleq",
                               "Qeqertalik",
                               "Qeqqata",
                               "Sermersooq"
                               )
                             )
         ) %>%
  arrange_all()

usethis::use_data(greenlanders, overwrite = TRUE)
