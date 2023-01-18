# Gem eksempel data fra statbanken
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

data_url <- "https://bank.stat.gl/api/v1/da/Greenland/BE/BE01/BE0120/BEXSTA.px"

bexstatest <- 
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
  arrange_all()

write_rds(bexstatest, bexstatest_rds_path)


# sas-dataset:
datasas <- read_sas("data-raw/bexstatest2.sas7bdat") %>% 
  rename(time=taar,value=antal,`place of
         birth`=fsted,gender=sex,age=alder,residence=bostedtyp)
write_rds(datasas, bexstatest2_rds_path)

