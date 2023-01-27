# Create test data sets
#
# Different data sources are stored in 'data-raw' and formatted data sets are in
# the folder 'data'.
library(tidyverse)
library(testthat)
library(readxl)
library(pxweb)
library(haven)

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
  arrange_all() %>%
  write_rds(test_path('fixtures', 'data', 'BEXSTA_small.rds'))

pxweb_get(url = "https://statbank.hagstova.fo:443/api/v1/fo/H2/DEV/COH/Lexis.px",
          query = list("event" = "*",
                       "sex"= "*",
                       "year"= as.character(2018:2021)
                       )
          ) %>%
  as.data.frame(column.name.type = "code",
                variable.value.type = "code"
                ) %>%
  rename(time = year, value = last_col()) %>%
  arrange_all() %>%
  write_rds(test_path('fixtures', 'data', 'FOTEST.rds'))

test_path('fixtures', 'data-raw', 'bexstatest2.sas7bdat') %>%
  read_sas() %>%
  rename(time = taar,
         value = antal,
         `place of birth` = fsted,
         gender = sex,
         age = alder,
         residence = bostedtyp
         ) %>%
  mutate(age = str_trim(age)) %>%
  write_rds(test_path('fixtures', 'data', 'BEXSTA_large.rds'))

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
  write_rds(test_path('fixtures', 'data', 'BEXLTALL.rds'))
