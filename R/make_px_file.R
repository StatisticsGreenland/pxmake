source(file.path('R', 'globals.R'))
source(file.path(helper_functions_file_path))

library(tidyverse)
library(readxl)

#' Create metadata for header in PX file
#' 
#' The metadata is generated from an Excel sheet and from the source data.
get_metadata <- function(table_name) {
  excel_metadata_path <- get_excel_metadata_path(table_name)
  
  if (!file.exists(excel_metadata_path)) {
    # Create metadata file from template
  }
  
  # Generate metadata from first sheet in Excel workbook.
  # Datasets starting with 'metadata_' are part of the final metadataset.
  variables <- 
    excel_metadata_path %>% 
    read_excel(sheet = "Variables_MD") %>% 
    mutate(VarName = str_lowercase_and_dot_as_space(VarName)) %>% 
    pivot_longer(cols = ends_with(c("_varName",
                                    "_note", 
                                    "_domain", 
                                    "_elimination"
                                    )
                                  ), 
    names_to = c("lang", "var_name"), 
    names_pattern = "(.*)_(.*)",
    ) %>%
    mutate(var_name = str_glue("l{var_name}")) %>% 
    pivot_wider(names_from = var_name, values_from = value) %>%
    mutate(lvarName = str_lowercase_and_dot_as_space(lvarName))
  
  time_values <-
    table_name %>% 
    get_source_data_path() %>% 
    read_rds() %>% 
    distinct(time) %>% 
    pull(1)
  
  metadata_time <-
    variables %>%
    filter(str_to_lower(type) == "time") %>% 
    mutate(keyword = add_language_to_keyword("VALUES", lang) %>% 
                     add_sub_key_to_keyword(lvarName),
           value = str_quote(time_values) %>% str_c(collapse = ',')
    ) %>%
    arrange(keyword) %>% 
    select(keyword, value)
  
  metadata_stub_and_head <-
    variables %>%
    drop_na(position) %>% 
    mutate(keyword = case_when(str_starts(position, 's') ~ 'STUB',
                               str_starts(position, 'h') ~ 'HEADING',
                               TRUE ~ NA_character_
                               ) %>%
                               add_language_to_keyword(lang)
    ) %>%
    arrange(position, keyword) %>% 
    group_by(keyword) %>%
    mutate(value = str_quote(lvarName) %>% str_c(collapse = ',')) %>% 
    ungroup() %>% 
    distinct(keyword, value)
  
  metadata_elimination <-
    variables %>% 
    drop_na(lelimination) %>%
    arrange(position) %>% 
    mutate(keyword = add_language_to_keyword("ELIMINATION", lang) %>% 
                     add_sub_key_to_keyword(lvarName),
           value   = str_quote(lelimination)
    ) %>%
    distinct(keyword, value)
  
  # Second sheet in Excel workbook.
  metadata_codes_values_precision <-
    excel_metadata_path %>% 
    read_excel(sheet = "Codelists_2MD") %>% 
    mutate(VarName = str_lowercase_and_dot_as_space(VarName)) %>% 
    pivot_longer(cols = ends_with("_codeLabel"), 
                 names_to = c("lang"),
                 names_pattern = "^(.*)_.*$"
                 ) %>% 
    left_join(variables %>% select(VarName, lang, lvarName),
              by = c("VarName", "lang")
              ) %>%
    pivot_longer(cols = c("code",  "value", "precision"),
                 names_to = "type"
                 ) %>%
    mutate(keyword = case_when(type == 'code'      ~ 'CODES',
                               type == 'value'     ~ 'VALUES',
                               type == 'precision' ~ 'PRECISION',
                               TRUE ~ NA_character_
                               ) %>% 
                               add_language_to_keyword(lang) %>%
                               add_sub_key_to_keyword(lvarName)
    ) %>% 
    arrange(keyword, sortorder) %>% 
    group_by(keyword) %>% 
    mutate(value = str_quote(value) %>% str_c(collapse = ',')) %>%
    ungroup() %>%
    drop_na(value) %>% 
    distinct(keyword, value) %>%
    relocate(keyword)
  
  # Third sheet in Excel workbook.
  metadata_general <-
    excel_metadata_path %>% 
    read_excel(sheet = "General_MD") %>%
    separate(keyword, 
             c("keyword", "lang"), 
             sep = "_(?=[en|da|kl])",
             fill = "right"
    ) %>%
    mutate(keyword = str_replace_all(keyword, "_", "-"),
           keyword = add_language_to_keyword(keyword, lang)
    ) %>% 
    arrange(!is.na(lang)) %>% 
    select(keyword, value)
  
  return(bind_rows(metadata_general, 
                   metadata_stub_and_head,
                   metadata_codes_values_precision,
                   metadata_time,
                   metadata_elimination
                   )
         )
}

get_data_cube <- function(table_name) {
  table_name %>% 
    get_source_data_path() %>%
    read_rds() %>% 
    as_tibble() %>% 
    pivot_wider(names_from = time, values_from = value) %>%
    mutate(`place of birth` = fct_relevel(`place of birth`, c("T", "N", "S")),
           gender = fct_relevel(gender, c("T", "M", "K"))
           ) %>% 
    arrange_all() %>%
    select(-`place of birth`, -gender)
}

format_px_data_as_lines <- function(metadata, data_cube) {
  metadata_lines <-
    str_c(metadata$keyword, 
          "=", 
          quote_unless_numeric_or_yes_no(metadata$value),
          ";"
          )
  
  data_lines <-
    data_cube %>% 
    unite(tmp, sep = " ") %>% 
    pull(tmp) 
    
  c(metadata_lines, "DATA=", data_lines, ";")
}

make_px_file <- function(table_name) {
  metadata  <- get_metadata(table_name)
  data_cube <- get_data_cube(table_name)
  
  px_lines <- format_px_data_as_lines(metadata, data_cube)
  
  write_lines(px_lines, file = get_px_file_path(table_name))
}

table_name <- "BEXSTATEST"
make_px_file(table_name)
