#*********************************
# Functions, perhaps to be included in a statgl package
#
# Purpose:
# to reuse codes and text for variables already in statbank
#
#*********************************

# --------------------------------------------------------------------
# getMetaFromStatbank (and testbank)
# statbank-Hagstova:
# "https://statbank.hagstova.fo:443/api/v1/fo/H2/DEV/COH/Lexis.px")
# --------------------------------------------------------------------

# hvordan kommer den til at virke med :
#, api_url = "https://bank.stat.gl/api/v1/en/Greenland"

#library(statgl)

# General load of variables, values - code and text - helpers
statgl_metatbl <- function(url) {
  url %>% 
    statgl_meta() %>% 
    pluck("variables") %>% 
    map(as_tibble) %>% 
    bind_rows() %>% 
    mutate(across(where(is.list), flatten_chr)) %>% 
    select(code:valueTexts)
}


metatbl_var <- function(varname,varnameout,px_id) {
  varBEXSTB <- langs %>% 
    map(statgl_url, table_id = px_id) %>% 
    map(statgl_metatbl) %>% 
    map2(langs, 
         ~ rename_at(
           .x, .vars = vars(-code, -values), 
           function(z) paste0(z, ".", .y)
         )
    ) %>% 
    reduce(left_join, by = c("code", "values")) %>% 
    filter(code==varname) %>% 
    mutate(variables=toupper(varnameout)) %>% 
    select(variables, values, everything()) %>% 
    select(VarName=variables,code=values,valueTexts.en,valueTexts.da,valueTexts.kl)
}

# CONST_statbank <- "https://bank.stat.gl/api/v1/en/Greenland"
# , api_url = CONST_statbank

metatbl_varname <- function(varname,px_id) {
  varBEXSTB <- langs %>% 
    map(statgl_url, table_id = px_id) %>% 
    map(statgl_metatbl) %>% 
    map2(langs, 
         ~ rename_at(
           .x, .vars = vars(-code, -values), 
           function(z) paste0(z, ".", .y)
         )
    ) %>% 
    reduce(left_join, by = c("code", "values"))
}  

