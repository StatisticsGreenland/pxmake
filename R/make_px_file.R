source(file.path('R', 'globals.R'))
source(file.path(helper_functions_file_path))

library(tidyverse)
library(readxl)

#' Read specific sheet in Excel metadata file
read_excel_metadata_sheet <- function(table_name, sheet_name) {
  table_name %>% 
    get_excel_metadata_path() %>% 
    read_excel(sheet = sheet_name)
}

#' Get metadata from first Excel sheet
get_variables_metadata <- function(table_name) {
  read_excel_metadata_sheet(table_name, "Variables_MD") %>% 
    mutate(VarName = str_to_lower(VarName)) %>% 
    pivot_longer(cols = ends_with(c("_varName",
                                    "_note", 
                                    "_domain", 
                                    "_elimination"
                                    )
                                  ),
                 names_to = c("lang", "var_name"),
                 names_pattern = "(.*)_(.*)"
                 ) %>%
    mutate(var_name = str_glue("l{var_name}")) %>% 
    pivot_wider(names_from = var_name, values_from = value)
}

#' Get metadata from second Excel sheet
get_codelist_metadata <- function(table_name) {
  read_excel_metadata_sheet(table_name, "Codelists_2MD") %>%
    mutate(VarName = str_to_lower(VarName)) %>%
    pivot_longer(cols = ends_with("_codeLabel"), 
                 names_to = c("lang"),
                 names_pattern = "^(.*)_.*$"
    )
}

#' Get metadata from third Excel sheet
get_general_metadata <- function(table_name) {
  read_excel_metadata_sheet(table_name, "General_MD") %>%
    separate(keyword, 
             c("keyword", "lang"), 
             sep = "_(?=[en|da|kl])",
             fill = "right"
             ) %>%
    mutate(keyword = str_replace_all(keyword, "_", "-"))
}

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
  variables <- get_variables_metadata(table_name)
  
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
  
    metadata_domain <-
    variables %>% 
    drop_na(ldomain) %>%
    arrange(position) %>% 
    mutate(keyword = add_language_to_keyword("DOMAIN", lang) %>% 
             add_sub_key_to_keyword(lvarName),
           value   = str_quote(ldomain)
    ) %>%
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
  
  metadata_note <-
    variables %>% 
    drop_na(lnote) %>%
    arrange(position) %>% 
    mutate(keyword = add_language_to_keyword("NOTE", lang) %>% 
             add_sub_key_to_keyword(lvarName),
           value   = str_quote(lnote)
    ) %>%
    distinct(keyword, value)

  # # Second sheet in Excel workbook.
  # Johans kode - hvor Precision ikke bliver rigtig
  
  # metadata_codes_values_precision <-
  #   get_codelist_metadata(table_name) %>% 
  #   left_join(variables %>% select(VarName, lang, lvarName),
  #             by = c("VarName", "lang")
  #   ) %>%
  #   pivot_longer(cols = c("code",  "value", "precision"),
  #                names_to = "type"
  #   ) %>%
  #   mutate(keyword = case_when(type == 'code'      ~ 'CODES',
  #                              type == 'value'     ~ 'VALUES',
  #                              type == 'precision' ~ 'PRECISION',
  #                              TRUE ~ NA_character_
  #   ) %>% 
  #     add_language_to_keyword(lang) %>%
  #     add_sub_key_to_keyword(lvarName)
  #   ) %>% 
  #   arrange(keyword, sortorder) %>% 
  #   group_by(keyword) %>% 
  #   mutate(value = str_quote(value) %>% str_c(collapse = ',')) %>%
  #   ungroup() %>%
  #   drop_na(value) %>% 
  #   distinct(keyword, value) %>%
  #   relocate(keyword)
  
  
    
  # Second sheet in Excel workbook.
  # Lars tilpasset kode - hvor Precision er rigtig
  metadata_codes_values_precision <-
    get_codelist_metadata(table_name) %>% 
    left_join(variables %>% select(VarName, lang, lvarName),
              by = c("VarName", "lang")
              ) %>%
    mutate(VarName2=paste0(lvarName,str_quote(","),value)) %>% 
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
    mutate(value = ifelse(type %in% c('code','value'),
                          str_quote(value) %>% str_c(collapse = ','),
           value)) %>%
    ungroup() %>%
    drop_na(value) %>% 
    mutate(keyword2 = case_when(type == 'precision' ~ 'PRECISION',
                               TRUE ~ NA_character_
    ) %>% 
      add_language_to_keyword(lang) %>%
      add_sub_key_to_keyword(VarName2)
    ) %>% 
    distinct(type, keyword,keyword2, value) %>%
    mutate(keyword = ifelse(type == 'precision',keyword2,keyword)) %>% 
    select(keyword,value) %>% 
    distinct(keyword,value) %>%
    relocate(keyword)
  
  # Third sheet in Excel workbook.
  metadata_general <-
    table_name %>% 
    get_general_metadata %>%
    mutate(keyword = add_language_to_keyword(keyword, lang)) %>% 
    arrange(!is.na(lang)) %>% 
    select(keyword, value)
  
  return(bind_rows(metadata_general, 
                   metadata_stub_and_head,
                   metadata_codes_values_precision,
                   metadata_time,
                   metadata_domain,
                   metadata_elimination,
                   metadata_note
                   )
         )
}

#' Create data cube from source and meta data
#' 
#' The data cube has one column for each value of HEADING and is sorted by 
#' value. There is one row for each combination of values of STUB variables. The
#' ordering of STUB variables are set in the metadata.
get_data_cube <- function(table_name) {
  variables <- 
    table_name %>% 
    get_variables_metadata() %>% 
    arrange(position)
  
  stub_vars <-
    variables %>%
    filter(str_starts(position, 's'), lang == "en") %>%
    pull(VarName)
  
  heading_var <- 
    variables %>% 
    filter(str_starts(position, 'h'), lang == "en") %>% 
    pull(VarName)
  
  if (length(heading_var) != 1) {
    # Technically more headings can be used, but this is not implemented.
    stop(str_glue("Need exactly 1 heading variable, there are: {length(heading_var)}."))
  }
  
  codelist <- 
    get_codelist_metadata(table_name) %>% 
    left_join(variables %>% select(VarName, lang, lvarName),
              by = c("VarName", "lang")
              ) %>%
    filter(lang == "en") %>% 
    select(VarName, sortorder, code)
  
  source_data <-
    table_name %>% 
    get_source_data_path() %>% 
    read_rds() %>%
    # Complete data for rows withAdd missing rows, for data 
    complete(!!!syms(heading_var), !!!syms(stub_vars))
  
  data_cube <-
    source_data %>%
    mutate(id = row_number()) %>% # used to unpivot data after sortorder is added
    pivot_longer(cols = all_of(stub_vars), 
                 names_to = "VarName",
                 values_to = "code"
                 ) %>%
    left_join(codelist, by = c("VarName", "code")) %>% 
    pivot_wider(names_from = VarName, 
                values_from = c("code", "sortorder")
                ) %>% 
    select(-id) %>% 
    arrange_at(heading_var) %>% 
    pivot_wider(names_from = !!heading_var, values_from = value) %>% 
    arrange_at(str_c("sortorder_", stub_vars)) %>% 
    select(-contains(stub_vars))
  
  return(data_cube)
}

#' Turn metadata and data cube into text lines that can be written to a px file.
format_px_data_as_lines <- function(metadata, data_cube) {
  metadata_lines <-
    str_c(metadata$keyword, 
          "=", 
          quote_unless_numeric_or_yes_no(metadata$value),
          ";"
          )
  
  data_lines <-
    data_cube %>% 
    mutate(across(everything(), as.character),
           across(everything(), ~replace_na(.x, '"-"'))
           ) %>% 
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

make_px_file("BEXSTATEST")
make_px_file("BEXSTATEST2")
make_px_file("BEXLTALL")
#make_px_file("BEXLTREG") - ikke med
