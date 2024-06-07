library(tidyverse)
library(pxmake)

population_path <- file.path('data-raw', 'population_gl.px')

# query: https://bank.stat.gl:443/sq/6e4a1010-278f-46f5-b185-8d2c5e79fee0

download.file("https://bank.stat.gl:443/sq/0cf06962-19f1-4d5c-8d43-b7ed0009617d",
              population_path
              )

population_gl <-
  population_path %>%
  px() %>%
  pluck("data") %>%
  rename(year = time, n = figures_) %>%
  mutate(gender = fct_recode(gender, male = "M", female = "K"))

usethis::use_data(population_gl, overwrite = TRUE)
