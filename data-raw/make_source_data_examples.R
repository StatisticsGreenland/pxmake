# Gem eksempel data fra statbanken og sas datasæt
#
# Tabel på bank.stat.gl:
# https://bank.stat.gl:443/sq/f36e3638-870f-4697-b001-ae1e9cb81abc
#
# Definitionsbeskrivelse:
# https://bank.stat.gl:443/sqget.asp?f36e3638-870f-4697-b001-ae1e9cb81abc
source(file.path('R', 'globals.R'))

library(tidyverse)
library(pxweb)
library(haven)
library(readxl)


# From Statbank Greenland 
#
data_url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BE0120/BEXSTA.px"

pxweb_get(url = data_url,
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
  write_rds(bexstatest_rds_path)



# From a sas7bdat dataset
#
read_sas("data-raw/bexstatest2.sas7bdat") %>% 
  rename(time=taar,
         value=antal,
         `place of birth`=fsted,
         gender=sex,
         age=alder,
         residence=bostedtyp
         ) %>%
  mutate(age = str_trim(age)) %>% 
  write_rds(bexstatest2_rds_path)

# From a dataset created in R
#
"BEXLTALL_RAW" %>% 
  get_source_data_path() %>% 
  read_rds() %>%
  rename(`place of birth`=pob,
         nop=nop.code,
         age=age.code,
         sex=sex.code,
         calcbase=calcbase.code,
         measure=measure.code) %>% 
  mutate(age=as.character(age)) %>% 
  filter(is.finite(value)) %>% 
  write_rds(bexltall_rds_path)


# "BEXLTREG_RAW" %>% 
#   get_source_data_path() %>% 
#   read_rds() %>%
#   rename(`place of birth`=pob,area=area.code,nop=nop.code,age=age.code,sex=sex.code,calcbase=calcbase.code,measure=measure.code) %>% 
#   mutate(age=as.character(age)) %>% 
#   write_rds(bexltreg_rds_path)


# From an Excel spreadsheet
#
# Read specific sheet in an Excel file
# sheet_name <- "Sample"
# Excelsample <- read_excel(path = excelsample_rds_path, sheet = sheet_name)
