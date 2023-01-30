#' Get metadata from first Excel sheet
get_variables_metadata <- function(metadata_path) {
  readxl::read_excel(metadata_path, sheet = "Variables_MD") %>%
    dplyr::mutate(VarName = stringr::str_to_lower(VarName)) %>%
    tidyr::pivot_longer(cols = ends_with(c("_varName",
                                           "_note",
                                           "_domain",
                                           "_elimination")),
                        names_to = c("lang", "var_name"),
                        names_pattern = "(.*)_(.*)"
                        ) %>%
    dplyr::mutate(var_name = stringr::str_glue("l{var_name}")) %>%
    tidyr::pivot_wider(names_from = var_name, values_from = value)
}

#' Get metadata from second Excel sheet
get_codelist_metadata <- function(metadata_path) {
  metadata_path %>%
    readxl::read_excel(sheet = "Codelists_2MD") %>%
    dplyr::mutate(VarName = stringr::str_to_lower(VarName)) %>%
    tidyr::pivot_longer(cols = ends_with("_codeLabel"),
                        names_to = c("lang"),
                        names_pattern = "^(.*)_.*$"
                        )
}

#' Get metadata from third Excel sheet
get_general_metadata <- function(metadata_path) {
  metadata_path %>%
    readxl::read_excel(sheet = "General_MD") %>%
    tidyr::separate(keyword,
                    c("keyword", "lang"),
                    sep = "_(?=[en|da|kl|fo])",
                    fill = "right"
                    ) %>%
    dplyr::mutate(keyword = stringr::str_replace_all(keyword, "_", "-"))
}

#' Create metadata for header in PX file
#'
#' The metadata is generated from an Excel sheet and from the source data.
get_metadata <- function(metadata_path, source_data_path) {
  # Generate metadata from first sheet in Excel workbook.
  # Datasets starting with 'metadata_' are part of the final metadataset.
  variables <- get_variables_metadata(metadata_path)

  time_values <-
    source_data_path %>%
    readRDS() %>%
    dplyr::distinct(time) %>%
    dplyr::pull(1)

  metadata_time <-
    variables %>%
    dplyr::filter(stringr::str_to_lower(type) == "time") %>%
    dplyr::mutate(keyword = add_language_to_keyword("VALUES", lang) %>%
                              add_sub_key_to_keyword(lvarName),
                  value = str_quote(time_values) %>%
                            stringr::str_c(collapse = ',')
                  ) %>%
    dplyr::arrange(keyword) %>%
    dplyr::select(keyword, value)

  metadata_stub_and_head <-
    variables %>%
    tidyr::drop_na(position) %>%
    dplyr::mutate(keyword =
                    dplyr::case_when(stringr::str_starts(position, 's') ~ 'STUB',
                                     stringr::str_starts(position, 'h') ~ 'HEADING',
                                     TRUE ~ NA_character_
                                     ) %>%
                                     add_language_to_keyword(lang)
                  ) %>%
    dplyr::arrange(position, keyword) %>%
    dplyr::group_by(keyword) %>%
    dplyr::mutate(value = str_quote(lvarName) %>% stringr::str_c(collapse = ',')) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(keyword, value)

  metadata_domain <-
    variables %>%
    tidyr::drop_na(ldomain) %>%
    dplyr::arrange(position) %>%
    dplyr::mutate(keyword = add_language_to_keyword("DOMAIN", lang) %>%
                              add_sub_key_to_keyword(lvarName),
                  value = str_quote(ldomain)
                  ) %>%
    dplyr::distinct(keyword, value)

  metadata_elimination <-
    variables %>%
    tidyr::drop_na(lelimination) %>%
    dplyr::arrange(position) %>%
    dplyr::mutate(keyword = add_language_to_keyword("ELIMINATION", lang) %>%
                              add_sub_key_to_keyword(lvarName),
                  value = str_quote(lelimination)
                  ) %>%
    dplyr::distinct(keyword, value)

  metadata_note <-
    variables %>%
    tidyr::drop_na(lnote) %>%
    dplyr::arrange(position) %>%
    dplyr::mutate(keyword = add_language_to_keyword("NOTE", lang) %>%
                              add_sub_key_to_keyword(lvarName),
                  value = str_quote(lnote)
                  ) %>%
    dplyr::distinct(keyword, value)

  # Second sheet in Excel workbook.
  metadata_codes_values_precision <-
    get_codelist_metadata(metadata_path) %>%
    dplyr::left_join(variables %>% dplyr::select(VarName, lang, lvarName),
                     by = c("VarName", "lang")
                     ) %>%
    dplyr::mutate(VarName2 = paste0(lvarName, str_quote(","), value)) %>%
    tidyr::pivot_longer(cols = c("code",  "value", "precision"),
                        names_to = "type"
                        ) %>%
    dplyr::mutate(keyword = dplyr::case_when(type == 'code'      ~ 'CODES',
                                             type == 'value'     ~ 'VALUES',
                                             type == 'precision' ~ 'PRECISION',
                                             TRUE ~ NA_character_
                                             ) %>%
                                             add_language_to_keyword(lang) %>%
                                             add_sub_key_to_keyword(lvarName)
                  ) %>%
    dplyr::arrange(keyword, sortorder) %>%
    dplyr::group_by(keyword) %>%
    dplyr::mutate(value = ifelse(type %in% c('code','value'),
                                 str_quote(value) %>% stringr::str_c(collapse = ','),
                                 value)
                  ) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(keyword2 = dplyr::case_when(type == 'precision' ~ 'PRECISION',
                                              TRUE ~ NA_character_
                                              ) %>%
                                              add_language_to_keyword(lang) %>%
                                              add_sub_key_to_keyword(VarName2)
                  ) %>%
    dplyr::distinct(type, keyword,keyword2, value) %>%
    dplyr::mutate(keyword = ifelse(type == 'precision', keyword2, keyword)) %>%
    # PRECISION after VALUES
    dplyr::arrange(keyword2) %>%
    dplyr::select(keyword, value) %>%
    dplyr::distinct(keyword, value) %>%
    dplyr::relocate(keyword)

  # Third sheet in Excel workbook.
  metadata_general <-
    get_general_metadata(metadata_path) %>%
    dplyr::mutate(keyword = add_language_to_keyword(keyword, lang)) %>%
    dplyr::arrange(!is.na(lang)) %>%
    dplyr::select(keyword, value)

  return(dplyr::bind_rows(metadata_general,
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
get_data_cube <- function(metadata_path, source_data_path) {
  variables <-
    get_variables_metadata(metadata_path) %>%
    dplyr::arrange(position)

  stub_vars <-
    variables %>%
    dplyr::filter(stringr::str_starts(position, 's'), lang == "en") %>%
    dplyr::pull(VarName)

  heading_var <-
    variables %>%
    dplyr::filter(stringr::str_starts(position, 'h'), lang == "en") %>%
    dplyr::pull(VarName)

  if (length(heading_var) != 1) {
    # Technically more headings can be used, but this is not implemented.
    msg <- "Need exactly 1 heading variable, there are: {length(heading_var)}."
    stop(stringr::str_glue(msg))
  }

  codelist <-
    get_codelist_metadata(metadata_path) %>%
    dplyr::left_join(variables %>% dplyr::select(VarName, lang, lvarName),
                     by = c("VarName", "lang")
                     ) %>%
    dplyr::filter(lang == "en") %>%
    dplyr::select(VarName, sortorder, code)

  source_data <-
    source_data_path %>%
    readRDS() %>%
    # Complete data so all values of all variables appear in all combinations
    tidyr::complete(!!!rlang::syms(heading_var), !!!rlang::syms(stub_vars))

  data_cube <-
    source_data %>%
    dplyr::mutate(id = dplyr::row_number()) %>% # used to unpivot data later
    tidyr::pivot_longer(cols = all_of(stub_vars),
                        names_to = "VarName",
                        values_to = "code"
                        ) %>%
    dplyr::left_join(codelist, by = c("VarName", "code")) %>%
    tidyr::pivot_wider(names_from = VarName,
                       values_from = c("code", "sortorder")
                       ) %>%
    dplyr::select(-id) %>%
    dplyr::arrange_at(heading_var) %>%
    tidyr::pivot_wider(names_from = !!heading_var, values_from = value) %>%
    dplyr::arrange_at(stringr::str_c("sortorder_", stub_vars)) %>%
    dplyr::select(-contains(stub_vars))

  return(data_cube)
}

#' Turn metadata and data cube into text lines that can be written to a px file.
format_px_data_as_lines <- function(metadata, data_cube) {
  metadata_lines <-
    stringr::str_c(metadata$keyword,
                   "=",
                   quote_unless_numeric_or_yes_no(metadata$value),
                   ";"
                   )

  data_lines <-
    data_cube %>%
    dplyr::mutate(dplyr::across(everything(), as.character),
                  dplyr::across(everything(), ~tidyr::replace_na(.x, '"-"'))
                  ) %>%
    tidyr::unite(tmp, sep = " ") %>%
    dplyr::pull(tmp)

  c(metadata_lines, "DATA=", data_lines, ";")
}

#' Create pxfile
#'
#' `pxmake()` creates a px file by combine source data from a `.rds` file and
#' metadata from an Excel workbook.
#'
#' @param source_data_path Path to `.rds` file with data source.
#' @param metadata_path Path to Excel workbook with metadata.
#' @param pxfile_path Path to save px file at.
#'
#' @return Nothing.
#'
#' @export
pxmake <- function(source_data_path, metadata_path, pxfile_path) {
  metadata  <- get_metadata(metadata_path, source_data_path)
  data_cube <- get_data_cube(metadata_path, source_data_path)

  px_lines <- format_px_data_as_lines(metadata, data_cube)

  file_connection <- file(pxfile_path)
  writeLines(px_lines, file_connection)
  close(file_connection)
}
