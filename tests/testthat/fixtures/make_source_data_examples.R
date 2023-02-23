# Create test data sets
#
# Different data sources are stored in 'data-raw' and formatted data sets are in
# the folder 'data'.
library(tidyverse)
library(testthat)
library(readxl)
library(pxweb)
library(haven)

bexsta <-
  pxweb_get(url = "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BE0120/BEXSTA.px",
          query = list("place of birth" = c("T","N","S"),
                       "gender"= "*",
                       "time"= as.character(2018:2022)
                       )
          ) %>%
  as.data.frame(column.name.type = "code",
                variable.value.type = "code"
                ) %>%
  rename(value = last_col()) %>%
  arrange_all()

write_rds(bexsta, test_path('fixtures', 'data', 'BEXSTA.rds'))

bexsta %>%
  filter(`place of birth` != 'T', gender != "T") %>%
  write_rds(test_path('fixtures', 'data', 'BEXSTA_WITHOUT_TOTALS.rds'))

test_path('fixtures', 'data-raw', 'BEXLTALL_RAW.rds') %>%
  read_rds() %>%
  rename(`place of birth` = pob,
         nop = nop.code,
         age = age.code,
         sex = sex.code,
         calcbase = calcbase.code,
         measure = measure.code
         ) %>%
  mutate(age = as.character(age)) %>%
  filter(is.finite(value)) %>%
  group_by(age) %>%
  write_rds(test_path('fixtures', 'data', 'BEXLTALL.rds'))
