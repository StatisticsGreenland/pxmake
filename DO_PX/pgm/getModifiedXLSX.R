
#*********************************
# 
# read modified Excelfile
# 
#
# with final metadata
#
# DO_PX_step2
# 
#*********************************

Mod_Variables_MD <- read_xlsx(paste0(metaPath,"/meta_",PXMATRIX,"_Modified.xlsx"),
                              "Variables_MD", 
                              col_names = TRUE) %>% 
  mutate(VarName=str_replace_all(VarName," ","."))

Mod_Codelists_2MD <- read_xlsx(paste0(metaPath,"/meta_",PXMATRIX,"_Modified.xlsx"),
                               "Codelists_2MD",
                               col_names = TRUE) %>% 
  as_tibble() %>% 
  arrange(VarName,sortorder) %>% 
  mutate(VarName=str_replace_all(VarName," ","."))

Mod_General_MD <- read_xlsx(paste0(metaPath,"/meta_",PXMATRIX,"_Modified.xlsx"),
                            "General_MD", 
                            col_names = TRUE)


tidy_Codelists <- Mod_Codelists_2MD %>%
  pivot_longer(cols=c(en_codeLabel,da_codeLabel,kl_codeLabel),names_to = "text") %>%
  mutate(lang=str_sub(text,1,2),
         VarName=str_to_lower(VarName)) %>% 
  select(lang,VarName,code,value,sortorder,precision)

tidy_General_noLang <- Mod_General_MD %>% as_tibble() %>% 
  mutate(lang=str_sub(keyword,-2),
         keyword=str_replace_all(keyword,"_","-")) %>% 
  filter(!lang %in% c("en","da","kl")) %>% 
  select(-lang) %>% 
  mutate(keyword=ifelse(keyword=="DECIMAL","DECIMALS",keyword),
         keyword=ifelse(keyword=="SHOWDECIMAL","SHOWDECIMALS",keyword))

tidy_General <- Mod_General_MD %>%
  mutate(lang=str_sub(keyword,-2,-1),
         keyword=str_sub(keyword,1,-4)) %>% 
         # keyword=ifelse((lang=="en"),
         #                keywordNEW,
         #                paste0(keywordNEW,"[",lang,"]"))
         # ) %>% 
  filter(lang %in% c("en","da","kl")) %>% 
  mutate(keyword=str_replace_all(keyword,"_","-"))



tidy_VarName <- Mod_Variables_MD %>% select(position,VarName,type,en_varName,da_varName,kl_varName) %>%
  pivot_longer(cols=c(en_varName,da_varName,kl_varName),names_to = "text") %>%
  mutate(lang=str_sub(text,1,2)) %>% 
  select(lang,VarName,type,position,lVarName=value)

tidy_VarNote <- Mod_Variables_MD %>% select(position,VarName,type,en_note,da_note,kl_note) %>%
  pivot_longer(cols=c(en_note,da_note,kl_note),names_to = "text") %>%
  mutate(lang=str_sub(text,1,2)) %>% 
  select(lang,VarName,type,position,lnote=value)

tidy_VarDomain <- Mod_Variables_MD %>% select(position,VarName,type,en_domain,da_domain,kl_domain) %>%
  pivot_longer(cols=c(en_domain,da_domain,kl_domain),names_to = "text") %>%
  mutate(lang=str_sub(text,1,2)) %>% 
  select(lang,VarName,type,position,ldomain=value)

tidy_VarElim <- Mod_Variables_MD %>% select(position,VarName,type,en_elimination,da_elimination,kl_elimination) %>%
  pivot_longer(cols=c(en_elimination,da_elimination,kl_elimination),names_to = "text") %>%
  mutate(lang=str_sub(text,1,2)) %>% 
  select(lang,VarName,type,position,lelimination=value)

tidy_Variables <- tidy_VarName %>% 
  left_join(tidy_VarNote) %>% 
  left_join(tidy_VarDomain) %>% 
  left_join(tidy_VarElim)

rm(tidy_VarElim,tidy_VarDomain,tidy_VarNote,tidy_VarName)

tidy_Codelists_lang <- tidy_Codelists %>% 
  left_join(tidy_Variables) %>% 
  mutate(CODES = paste0('CODES[',lang,']("',lVarName,'")'),
         VALUES = paste0('VALUES[',lang,']("',lVarName,'")'))

tidy_precision <- tidy_Codelists %>% drop_na(precision) %>% 
  left_join(tidy_Variables) %>% 
  mutate(keyword=paste0('PRECISION[',lang,']("',lVarName,'","',value,'")')) %>% 
  select(keyword,value=precision)

tidy_Codelists_sort <- tidy_Codelists_lang %>% 
  select(lang,VarName,lVarName,code,value,tmp_sortorder=sortorder) %>% 
  mutate(sortorder=ifelse(is.na(tmp_sortorder),code,tmp_sortorder)) %>% 
  select(-tmp_sortorder)

