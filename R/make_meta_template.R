
source(file.path('R', 'globals.R'))
source(file.path(helper_functions_file_path))

library(tidyverse)
library(jsonlite)
library(haven)

# 
# Purpose:
# When a tidy dataset is ready to be transformed to a px-file, it only
# holds codes for variablenames and codelists.

# sas-dataset:
datasas <- read_sas("data-raw/bexstatest.sas7bdat") %>% 
  filter(strtoi(taar)>=2018)

#   Multilingual texts and additional metadata needs to be defined.
# The Statbank itself is used as a repository, as a lot of metadata
# is allready translated between english/danish/greenlandic. At the 
# same time reusing codes and texts helps to standardize between tables.
#
# A variable 'pob' needs to be presented as 'place of birth' and is found
# in many population tables. For instance table: BEXSTA
#
# 




# url <- "https://bank.stat.gl/api/v1/da/Greenland/KR/KRXAF.px"
# data <- fromJSON(url)
# da <- as.data.frame(data)
# 
# urlPathId <- "https://bank.stat.gl/api/v1/en/Greenland?query=KRXAF"
# data <- fromJSON(urlPathId)
# en <- as.data.frame(data)

varget <- function(matrix,lang) {
  tmp <- fromJSON(paste0("https://bank.stat.gl/api/v1/",lang,"/Greenland?query=",matrix))
  varipx <- fromJSON(paste0("https://bank.stat.gl/api/v1/",lang,"/Greenland",tmp$path,"/",tmp$id))
  return(varipx)
}
variablesintable <- varget("BEXSTA","da")

varget2 <- function(matrix,lang) {
  tmp <- fromJSON(glue::glue("https://bank.stat.gl/api/v1/{lang}/Greenland?query=",matrix))
  varipx <- fromJSON(glue::glue("https://bank.stat.gl/api/v1/{lang}/Greenland",tmp$path,"/",tmp$id))
  return(varipx)
}

# variablesintable2en <- varget2("BEXSTA","en")

lang <- c("en","da","kl")

hi <- lang %>% map_df(~ varget2("BEXSTD",.x),.id="langcode") %>% 
  rbind()


# # Eksempel fra Færøerne
# # "https://statbank.hagstova.fo/api/v1/en/H2"
# 
# sbf <- "https://statbank.hagstova.fo/api/v1/en/H2"
# 
# fo_url <- paste0(sbf,"?query=","Lexis")
# data <- fromJSON(fo_url)
# sbf_tmp <- as.data.frame(data) %>% filter(id=="Lexis.px")
# 
# fo_url2 <- paste0(sbf,sbf_tmp$path,"/",sbf_tmp$id)
# data <- fromJSON(fo_url2)
# sbf_info <- as.data.frame(data)
