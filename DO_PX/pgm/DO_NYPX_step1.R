#*******************************************************
#
# DO_PX, step 1
# 
# Do a draft metadata-xlsx file. When final edited in Excel, '_Modified' is added to file title
#
#*******************************************************

# loaded in test.Rmd:
# library(tidyverse)
# library(statgl)
# source(file.path(pgmPath,"getMetaFromStatbank.R"))



#*******************************************************
#
# Time as only variable in heading
# 

timeName <- tibble(code="time",
                   position="h1",
                   text.en="time",
                   text.da="tid",
                   text.kl="piffissaq",
                   type="time",
                   en_note="",
                   da_note="",
                   kl_note="",
                   en_elimination="",
                   da_elimination="",
                   kl_elimination="",
                   en_domain="",
                   da_domain="",
                   kl_domain="") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination,da_elimination,kl_elimination,en_domain,da_domain,kl_domain)


#*******************************************************
#
# stub variables
# 


#*******************************************************
#
# Place of Birth (pob)
# 

pobName <- metatbl_varname("place of birth","BEXSTA") %>%
  filter(code=="place of birth" & values=="T") %>%
  mutate(position="s1",
         type="2MD",
         en_note="",
         da_note="",
         kl_note="",
         en_domain="",
         da_domain="",
         kl_domain="") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination=valueTexts.en,da_elimination=valueTexts.da,kl_elimination=valueTexts.kl,en_domain,da_domain,kl_domain)

pob <- metatbl_var("place of birth","place of birth","BEXSTA") %>%
  mutate(sortorder="",
         precision="")



#*******************************************************
#
# gender
# 

genderName <- metatbl_varname("age","BEXSTA") %>% 
  filter(code=="gender" & values=="T") %>%
  mutate(position="s4",
         type="2MD",
         en_note="",
         da_note="",
         kl_note="",
         en_domain="",
         da_domain="",
         kl_domain="",
         en_elimination="YES",
         da_elimination="YES",
         kl_elimination="YES") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination,da_elimination,kl_elimination,en_domain,da_domain,kl_domain)


gender <- metatbl_var("gender","gender","BEXSTA") %>% 
  mutate(sortorder="",
         precision="",
         code = str_to_upper(code),
         code = case_when(code == "K" ~ "F", TRUE ~ code)
  )







#*******************************************************
#
# value
# 

valueName <- tibble(code="value",
                    position="",
                    text.en="",
                    text.da="",
                    text.kl="",
                    type="FIGURES",
                    en_note="",
                    da_note="",
                    kl_note="",
                    en_elimination="",
                    da_elimination="",
                    kl_elimination="",
                    en_domain="",
                    da_domain="",
                    kl_domain="") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination,da_elimination,kl_elimination,en_domain,da_domain,kl_domain)

#
# variables ready
#
#
#*******************************************************
#
# create dataframe for Variables_MD in meta-xlsx-file
# 



variables_MD <- timeName %>% 
  rbind(pobName) %>% 
  rbind(genderName) %>% 
  as.data.frame() %>% 
  rename(en_varName=text.en,da_varName=text.da,kl_varName=text.kl)


codelists_2MD <- pob %>% 
  rbind(gender) %>%
  as.data.frame() %>% 
  rename(en_codeLabel=valueTexts.en,da_codeLabel=valueTexts.da,kl_codeLabel=valueTexts.kl)


#*********************************
# 
# general meta
# 
#*********************************

General_MD <- setNames(
  data.frame(
    t(data.frame(c("CONTACT_en","Lars Pedersen, LARP@stat.gl"),
                 c("CONTACT_da","Lars Pedersen, LARP@stat.gl"),
                 c("CONTACT_kl","Lars Pedersen, LARP@stat.gl"),
                 c("NOTE_da","Hej Johan"),
                 c("NOTE_en","Hej Johan"),
                 c("NOTE_kl","Hej Johan"),
                 c("CONTENTS_da","Test befolkning 2018-"),
                 c("CONTENTS_en","Test Population, 2018-"),
                 c("CONTENTS_kl","kl Test px, 2018-"),
                 c("UNITS_da","antal"),
                 c("UNITS_en","number"),
                 c("UNITS_kl","kl antal"),
                 c("SOURCE_da","Grønlands Statistik"),
                 c("SOURCE_kl","Naatsorsueqqissaartarfik"),
                 c("SOURCE_en","Statistics Greenland"),
                 c("SUBJECT_CODE","BE"),
                 c("SUBJECT_AREA_da","Befolkning"),
                 c("SUBJECT_AREA_en","Population"),
                 c("SUBJECT_AREA_kl","Innuttaasut"),
                 c("DESCRIPTION_en","Test tabel, 2018- <em>[BEETestNy]</em>"),
                 c("DESCRIPTION_da","Test, 2018- <em>[BEDTestNy]</em>"),
                 c("DESCRIPTION_kl","kl Test 2018- <em>[BENTestNy]</em>"),
                 c("CREATION_DATE","20190124 09:00"),
                 c("UPDATE_FREQUENCY","Annually"),
                 c("LAST_UPDATE","20220222 09:00"),
                 c("NEXT_UPDATE","2023022210 09:00"),
                 c("DECIMAL",8),
                 c("SHOWDECIMAL",0),
                 c("LINK_da","www.stat.gl/bed202201/m1"),
                 c("LINK_en","www.stat.gl/bee202201/m1"),
                 c("LINK_kl","www.stat.gl/ben202201/m1")
    ))
    ,row.names = NULL,stringsAsFactors = FALSE
  ), 
  c("keyword","value")
)



#*********************************
# 
# Create Excelfil
# 
#*********************************


write_xlsx(list(Variables_MD = variables_MD, 
                Codelists_2MD = codelists_2MD, 
                General_MD = General_MD), 
           file.path(metaPath,"meta_",PXMATRIX,".xlsx"))


