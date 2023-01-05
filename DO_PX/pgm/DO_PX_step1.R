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
# source(getMetaFromStatbank.R")



#*******************************************************
#
# Time as only variable in heading
# 

timeName <- metatbl_varname("time","BEXCALCR") %>%
  mutate(values=strtoi(values)) %>% 
  filter(code=="time") %>% 
  filter(values==max(values)) %>%
  mutate(position="h1",
         type="time",
         en_note="",
         da_note="",
         kl_note="",
         en_domain="",
         da_domain="",
         kl_domain="",
         en_elimination="",
         da_elimination="",
         kl_elimination="") %>%
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
# nop - number of periods
# 

nopName <- tibble(code="nop",
                  position="s2",
                  text.en="no of years",
                  text.da="antal år i basis",
                  text.kl="kl antal år i basis",
                  type="2MD",
                  en_note="",
                  da_note="",
                  kl_note="",
                  en_elimination="5 years",
                  da_elimination="5 år",
                  kl_elimination="kl 5 år",
                  en_domain="",
                  da_domain="",
                  kl_domain="") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination,da_elimination,kl_elimination,en_domain,da_domain,kl_domain)

nop <- tibble(
  VarName = c("nop","nop","nop"),
  code = c("nop1","nop2","nop5"),
  valueTexts.en = c("1 year","2 years","5 years"),
  valueTexts.da = c("1 år","2 år","5 år"),
  valueTexts.kl = c("kl 1 år","kl 2 år","kl 5 år")) %>% 
  mutate(sortorder = c("1","2","3"),
         precision = "")



#*******************************************************
#
# Age
# 

ageName <- metatbl_varname("age","BEXSTA") %>% 
  filter(code=="age" & values=="20") %>%
  mutate(position="s3",
         type="2MD",
         en_note="",
         da_note="",
         kl_note="",
         en_domain="VPAge",
         da_domain="VPAlder",
         kl_domain="VPUkiuian",
         en_elimination="YES",
         da_elimination="YES",
         kl_elimination="YES") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination,da_elimination,kl_elimination,en_domain,da_domain,kl_domain)


age <- metatbl_var("age","age","BEXSTA") %>% 
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
# calcbase
# 

calcbaseName <- tibble(code="calcbase",
                       position="s5",
                       text.en="calc type",
                       text.da="beregningsgrundlag",
                       text.kl="kl beregningsgrundlag",
                       type="2MD",
                       en_note="",
                       da_note="",
                       kl_note="",
                       en_elimination="parallelogram",
                       da_elimination="b",
                       kl_elimination="kl b",
                       en_domain="",
                       da_domain="",
                       kl_domain="") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination,da_elimination,kl_elimination,en_domain,da_domain,kl_domain)

calcbase <- tibble(
  VarName = c("calcbase","calcbase"),
  code = c("A","B"),
  valueTexts.en = c("lexis squares","lexis parallelogram"),
  valueTexts.da = c("lexis a-grupper","lexis b-grupper"),
  valueTexts.kl = c("kl a","kl b")) %>% 
  mutate(sortorder = c("1","2"),
         precision = "")


#*******************************************************
#
# measure
# 

measureName <- tibble(code="measure",
                      position="s6",
                      text.en="measure",
                      text.da="dødelighedsmål",
                      text.kl="kl dødelighedsmål",
                      type="2MD",
                      en_note="",
                      da_note="",
                      kl_note="",
                      en_elimination="Life expectancy",
                      da_elimination="Middellevetid",
                      kl_elimination="kl Middellevetid",
                      en_domain="",
                      da_domain="",
                      kl_domain="") %>%
  select(position,VarName=code,text.en,text.da,text.kl,type,en_note,da_note,kl_note,en_elimination,da_elimination,kl_elimination,en_domain,da_domain,kl_domain)

measure <- tibble(
  VarName = c("measure","measure","measure","measure"),
  code = c("mx","qx","lx","ex"),
  valueTexts.en = c("Deathrate","Mortalityrate","Survivors","Life expectancy"),
  valueTexts.da = c("Dødskvotient","Dødshyppighed","Overlevende","Middellevetid"),
  valueTexts.kl = c("kl Dødskvotient","kl Dødshyppighed","kl Overlevende","kl Middellevetid")) %>% 
  mutate(sortorder = c("1","2","3","4"),
         precision = c("4","4","","1"))



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
  rbind(nopName) %>% 
  rbind(ageName) %>% 
  rbind(genderName) %>% 
  rbind(calcbaseName) %>% 
  rbind(measureName) %>% 
  as.data.frame() %>% 
  rename(en_varName=text.en,da_varName=text.da,kl_varName=text.kl)


codelists_2MD <- pob %>% 
  rbind(nop) %>% 
  rbind(age) %>% 
  rbind(gender) %>%
  rbind(calcbase) %>% 
  rbind(measure) %>% 
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
                 c("NOTE_da",""),
                 c("NOTE_en",""),
                 c("NOTE_kl",""),
                 c("CONTENTS_da","Overlevelsestavler 1990-"),
                 c("CONTENTS_en","Life tables, 1990-"),
                 c("CONTENTS_kl","kl Overlevelsestavler, 1990-"),
                 c("UNITS_da","kvotienter"),
                 c("UNITS_en","rates"),
                 c("UNITS_kl","kl kvotienter"),
                 c("SOURCE_da","Grønlands Statistik"),
                 c("SOURCE_kl","Naatsorsueqqissaartarfik"),
                 c("SOURCE_en","Statistics Greenland"),
                 c("SUBJECT_CODE","BE"),
                 c("SUBJECT_AREA_da","Befolkning"),
                 c("SUBJECT_AREA_en","Population"),
                 c("SUBJECT_AREA_kl","Innuttaasut"),
                 c("DESCRIPTION_en","Life tables, 1990- <em>[BEEBBLT]</em>"),
                 c("DESCRIPTION_da","Overlevelsestavler, 1990- <em>[BEDBBLT]</em>"),
                 c("DESCRIPTION_kl","kl Overlevelsestavler 1990- <em>[BENBBLT]</em>"),
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
           paste0(metaPath,"meta_",PXMATRIX,".xlsx"))


