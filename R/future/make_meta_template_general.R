#
# General_md
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

pxcontact <- "Lars Pedersen, LARP@stat.gl"
pxsubject <- "BE"
pxpublicdate <- "20230211 09:00"   # Expected release date
pxnextdate <- "20230211 09:00"     # Expected following release date
pxtablename <- ""
pxdecimals <- 15
pxshowdecimals <- 0

pxdescription_en <- "Life tables, 1982 - <em>[BEELTALL]</em>"
pxdescription_da <- "Overlevelsestavler, 1982 - <em>[BEDLTALL]</em>"
pxdescription_kl <- "kl Overlevelsestavler 1982 - <em>[BENLTALL]</em>"

pxmethods_da <- "www.stat.gl/bed202201/m1"
pxmethods_en <- "www.stat.gl/bee202201/m1"
pxmethods_kl <- "www.stat.gl/ben202201/m1"

pxnote_da <- ""
pxnote_en <- ""
pxnote_kl <- ""

pxnotex_da <- ""
pxnotex_en <- ""
pxnotex_kl <- ""

pxmainlanguage <- "en"
pxotherlanguages <- "en\",\"da\",\"kl"


source(file.path('R', 'globals.R'))
source(file.path(helper_functions_file_path))

library(tidyverse)
library(jsonlite)

subjectget <- function(lang) {
    subarea <- fromJSON(glue::glue(repository))
    return(subarea)
  }

repository <- "https://bank.stat.gl/api/v1/{lang}/Greenland"
lang <- c("en","da","kl")

subarea <- lang %>% map_df(~ subjectget(.x),.id="langcode") %>%
    filter(id=="BE")

CHARSET <- "ANSI"
AXIS_VERSION <- "2000"
LANGUAGE <- pxmainlanguage
LANGUAGES <- pxotherlanguages
MATRIX <- pxtablename
CONTACT_en <- pxcontact
CONTACT_da <- pxcontact
CONTACT_kl <- pxcontact
NOTE_da <- pxnote_da
NOTE_en <- pxnote_en
NOTE_kl <- pxnote_kl
CONTENTS_da <- "Overlevelsestavler"
CONTENTS_en <- "Overlevelsestavler"
CONTENTS_kl <- "kl Overlevelsestavler"
UNITS_da <- "kvotienter"
UNITS_en <- "rates"
UNITS_kl <- "kl kvotienter"
SOURCE_da <- "Grønlands Statistik"
SOURCE_kl <- "Naatsorsueqqissaartarfik"
SOURCE_en <- "Statistics Greenland"
SUBJECT_CODE <- pxsubject
SUBJECT_AREA_en <- subarea %>% filter(subarea$id==pxsubject & langcode=="1") %>% pull(text)
SUBJECT_AREA_da <- subarea %>% filter(subarea$id==pxsubject & langcode=="2") %>% pull(text)
SUBJECT_AREA_kl <- subarea %>% filter(subarea$id==pxsubject & langcode=="3") %>% pull(text)
DESCRIPTION_en <- pxdescription_en
DESCRIPTION_da <- pxdescription_da
DESCRIPTION_kl <- pxdescription_kl
CREATION_DATE <- "20190124 09:00"
UPDATE_FREQUENCY <- "Annually"
LAST_UPDATED <- pxpublicdate
NEXT_UPDATE <- pxnextdate
DECIMALS <- pxdecimals
SHOWDECIMALS <- pxshowdecimals
LINK_da <- pxmethods_da
LINK_en <- pxmethods_en
LINK_kl <- pxmethods_kl
NOTEX_da <- pxnotex_da
NOTEX_en <- pxnotex_en
NOTEX_kl <- pxnotex_kl


