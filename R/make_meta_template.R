#
# Every px-file is generated from a dataset with only codes and one value column
# Metadata for these data are kept in an Excel spreadsheet which
# dopxR combine to the px-file.
#
# This can all be done from scratch or setting out from an allready
# existing metadata-xlsx-file
#
# This script acknowledge that a lot of translated metadata allready
# exists in a nearby StatBank.
#
# This script set out to help build a metadata-xlsx-file with as
# much metadata as possible, to minimize the manual task in 
# finalizing the metadata-xlsx 
#

source(file.path('R', 'globals.R'))
source(file.path(helper_functions_file_path))

library(tidyverse)
library(jsonlite)

varget <- function(matrix,lang) {
  tmp <- fromJSON(glue::glue(repository,"?query=",matrix))
  varipx <- fromJSON(glue::glue(repository,tmp$path,"/",tmp$id))
  return(varipx)
}

#
# repositories
# Statbank Greenland

repository <- "https://bank.stat.gl/api/v1/{lang}/Greenland"
lang <- c("en","da","kl")

BEXSTC <- lang %>% map_df(~ varget("BEXSTC",.x),.id="langcode") %>% 
  rbind()

print(BEXSTC$variables$text)

age <- BEXSTC %>%  filter(BEXSTC$variables$code=="age")


UDXFDS <- lang %>% map_df(~ varget("UDXFDS",.x),.id="langcode") %>% 
  rbind()

print(UDXFDS$variables$text)

school <- UDXFDS %>%  filter(UDXFDS$variables$code=="school")

# Here variables age and school has been selected - to be formated as
# metadata.xlsx
to_metadata_xlsx <- age %>% 
  rbind(school)


# Hagstova
repository <- "https://statbank.hagstova.fo/api/v1/{lang}/H2"
lang <- c("en","fo")

Lexis.px <- lang %>% map_df(~ varget("Lexis.px",.x),.id="langcode") %>% 
  rbind()

event <- Lexis.px %>% filter(Lexis.px$variables$code=="event")
