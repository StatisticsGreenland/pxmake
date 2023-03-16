#' Get metadata from 'Table' sheet in Excel Workbook
#'
#' @param metadata_path Path to metadata file
get_table_metadata <- function(metadata_path) {
  metadata_path %>%
    readxl::read_xlsx(sheet = "Table") %>%
    tidyr::separate(keyword,
                    c("keyword", "language"),
                    sep = "_(?=[en|da|kl|fo])",
                    fill = "right"
                    )
}

#' Get metadata from 'Variables' sheet in Excel Workbook
#'
#' @param metadata_path Path to metadata file
get_variables_metadata <- function(metadata_path) {
  metadata_path %>%
    readxl::read_xlsx(sheet = "Variables") %>%
    tidyr::pivot_longer(cols = -c(position, variable, type),
                        names_to = c("language", "long_name"),
                        names_pattern = "^([[:alpha:]]*)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = long_name, values_from = value)
}

#' Get metadata from 'Codelists' sheet in Excel Workbook
#'
#' @param metadata_path Path to metadata file
get_codelist_metadata <- function(metadata_path) {
  metadata_path %>%
    readxl::read_xlsx(sheet = "Codelists") %>%
    tidyr::pivot_longer(cols = ends_with("_code_label"),
                        names_to = c("language"),
                        names_pattern = "^([[:alpha:]]*)_.*$"
                        )
}

#' Sort metadata keywords in recommended order
#'
#' @param metadata Data frame with metadata.
#'
#' @returns data frame
sort_metadata <- function(metadata) {
  metadata %>%
    dplyr::mutate(tmp_keyword = stringr::str_extract(keyword, "[[:upper:]-]+")) %>%
    dplyr::left_join(get_px_keywords()[c('keyword', 'order')],
                     by = c('tmp_keyword' = 'keyword')
                     ) %>%
    dplyr::arrange(order, keyword) %>%
    dplyr::select(-tmp_keyword, -order)
}

#' Create metadata for header in PX file
#'
#' The metadata is generated from an Excel sheet and from the source data.
#'
#' @param metadata_path Path to metadata
#' @param source_data_path Path to source data
get_metadata <- function(metadata_path, source_data_path) {
  # Generate metadata from first sheet in Excel workbook.
  # Datasets starting with 'metadata_' are part of the final metadataset.
  variables <- get_variables_metadata(metadata_path)

  time_variable <-
    variables %>%
    dplyr::filter(tolower(type) == "time") %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable)

  if(length(time_variable) > 0) {
    error_if_more_than_one_time_variable(time_variable)

    time_values <-
      get_source_data(source_data_path, metadata_path) %>%
      dplyr::distinct(across(all_of(time_variable))) %>%
      dplyr::pull(1)

    metadata_time_values <-
      variables %>%
      dplyr::filter(variable == time_variable) %>%
      dplyr::mutate(keyword = "VALUES" %>%
                      add_language_to_keyword(language) %>%
                      add_sub_key_to_keyword(long_name),
                    value = time_values %>%
                      str_quote() %>%
                      stringr::str_c(collapse = ',')
      ) %>%
      dplyr::select(keyword, value)

    metadata_codes <-
      variables %>%
      dplyr::filter(variable == time_variable) %>%
      dplyr::mutate(keyword = "CODES" %>%
                      add_language_to_keyword(language) %>%
                      add_sub_key_to_keyword(long_name),
                    value = time_values %>%
                      str_quote() %>%
                      stringr::str_c(collapse = ',')
      ) %>%
      dplyr::select(keyword, value)

    metadata_timeval <-
      variables %>%
      dplyr::filter(variable == time_variable) %>%
      dplyr::mutate(keyword = "TIMEVAL" %>%
                      add_language_to_keyword(language) %>%
                      add_sub_key_to_keyword(long_name),
                    value = paste0("TLIST(",
                                   get_timeval_type_from_values(time_values),
                                   "1),",
                                   time_values %>%
                                     stringr::str_replace_all('[:alpha:]', '') %>%
                                     str_quote() %>%
                                     stringr::str_c(collapse = ',')
                    )
      ) %>%
      dplyr::select(keyword, value)

    metadata_time <- dplyr::bind_rows(metadata_time_values,
                                      metadata_timeval,
                                      metadata_codes
                                      )
  } else {
    metadata_time <- NULL
  }

  metadata_stub_and_head <-
    variables %>%
    tidyr::drop_na(position) %>%
    dplyr::mutate(keyword =
                    dplyr::case_when(substr(tolower(position), 1, 1) == 's' ~ 'STUB',
                                     substr(tolower(position), 1, 1) == 'h' ~ 'HEADING',
                                     TRUE ~ NA_character_
                                     ) %>%
                                     add_language_to_keyword(language)
                  ) %>%
    dplyr::arrange(position, keyword) %>%
    dplyr::group_by(keyword) %>%
    dplyr::mutate(value = str_quote(long_name) %>% stringr::str_c(collapse = ',')) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(keyword, value)

  metadata_variables <-
    variables %>%
    tidyr::pivot_longer(cols = intersect(c("note", "domain", "elimination"),
                                         names(.)
                                         )
                        ) %>%
    tidyr::drop_na(value) %>%
    dplyr::arrange(name, position) %>%
    dplyr::mutate(keyword = toupper(name) %>%
                              add_language_to_keyword(language) %>%
                              add_sub_key_to_keyword(long_name),
                  value = str_quote(value)
                  ) %>%
    dplyr::select(keyword, value)

  metadata_codes_values_precision <-
    get_codelist_metadata(metadata_path) %>%
    dplyr::left_join(variables %>% dplyr::select(variable, language, long_name),
                     by = c("variable", "language")
                     ) %>%
    dplyr::mutate(VarName2 = paste0(long_name, str_quote(","), value)) %>%
    tidyr::pivot_longer(cols = c("code",  "value", "precision"),
                        names_to = "type"
                        ) %>%
    dplyr::mutate(keyword = dplyr::case_when(type == 'code'      ~ 'CODES',
                                             type == 'value'     ~ 'VALUES',
                                             type == 'precision' ~ 'PRECISION',
                                             TRUE ~ NA_character_
                                             ) %>%
                                             add_language_to_keyword(language) %>%
                                             add_sub_key_to_keyword(long_name)
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
                                              add_language_to_keyword(language) %>%
                                              add_sub_key_to_keyword(VarName2)
                  ) %>%
    dplyr::distinct(type, keyword,keyword2, value) %>%
    dplyr::mutate(keyword = ifelse(type == 'precision', keyword2, keyword)) %>%
    dplyr::distinct(keyword, value) %>%
    dplyr::relocate(keyword)

  metadata_table <-
    get_table_metadata(metadata_path) %>%
    dplyr::mutate(keyword = add_language_to_keyword(keyword, language),
                  value = tidyr::replace_na(value, "")
                  ) %>%
    dplyr::arrange(!is.na(language)) %>%
    dplyr::select(keyword, value)

  return(dplyr::bind_rows(metadata_table,
                          metadata_stub_and_head,
                          metadata_codes_values_precision,
                          metadata_time,
                          metadata_variables
                          ) %>% sort_metadata()
         )
}

#' Get source data
#'
#' @param source_data_path Path to source data
#' @param metadata_path Path to metadata.
#'
#' @returns Data frame
get_source_data <- function(source_data_path, metadata_path) {
  figures_var <- get_figures_variable(metadata_path)

  source_data_path %>%
    readRDS() %>%
    dplyr::ungroup() %>%
    # Change all variables without figures to character
    dplyr::mutate(across(-one_of(figures_var), as.character))
}

#' Create data cube from source and meta data
#'
#' The data cube has one column for each value of HEADING and is sorted by
#' value. There is one row for each combination of values of STUB variables. The
#' ordering of STUB variables are set in the metadata.
#'
#' @param metadata_path Path to metadata.
#' @param source_data_path Path to source data
get_data_cube <- function(metadata_path, source_data_path) {
  variables <-
    get_variables_metadata(metadata_path) %>%
    dplyr::arrange(position)

  stub_vars <-
    variables %>%
    dplyr::filter(substr(tolower(position), 1, 1) == 's', language == "en") %>%
    dplyr::pull(variable)

  heading_vars <-
    variables %>%
    dplyr::filter(substr(tolower(position), 1, 1) == 'h', language == "en") %>%
    dplyr::pull(variable)

  codelist <-
    get_codelist_metadata(metadata_path) %>%
    dplyr::left_join(variables %>% dplyr::select(variable, language, long_name),
                     by = c("variable", "language")
                     ) %>%
    dplyr::filter(language == "en") %>%
    dplyr::select(variable, sortorder, code)

  # Complete data by adding all combinations of variable values in data and
  # codelist
  tmp_source_data <- get_source_data(source_data_path, metadata_path)

  source_data_values <-
    tmp_source_data %>%
    dplyr::select(dplyr::all_of(c(heading_vars, stub_vars))) %>%
    lst_distinct_and_arrange()

  codelist_values <-
    split(codelist$code, codelist$variable) %>%
    lst_distinct_and_arrange()

  data_values <- merge_named_lists(source_data_values, codelist_values)

  source_data <- tidyr::complete(tmp_source_data, !!!data_values)

  data_cube <-
    source_data %>%
    dplyr::mutate(id = dplyr::row_number()) %>% # used to unpivot data later
    tidyr::pivot_longer(cols = all_of(c(heading_vars, stub_vars)),
                        names_to = "variable",
                        values_to = "code"
                        ) %>%
    dplyr::left_join(codelist, by = c("variable", "code")) %>%
    tidyr::pivot_wider(names_from = variable,
                       values_from = c("code", "sortorder")
                       ) %>%
    dplyr::select(-id) %>%
    # Sort by sortorder for first heading var, codes for second heading var,
    # sortorder for second heading var, etc.
    dplyr::arrange(dplyr::across(zip_vectors(paste0("sortorder_", heading_vars),
                                             paste0("code_", heading_vars)
                                             )
                                 )
                   ) %>%
    dplyr::select(-paste0("sortorder_", heading_vars)) %>%
    tidyr::pivot_wider(names_from = !!paste0("code_", heading_vars),
                       values_from = get_figures_variable(metadata_path)
                       ) %>%
    dplyr::arrange(dplyr::across(zip_vectors(paste0("sortorder_", stub_vars),
                                             paste0("code_", stub_vars)
                                             )
                                 )
                   ) %>%
    dplyr::select(-ends_with(paste0("_", stub_vars)))

  return(data_cube)
}

#' Turn metadata and data cube into text lines that can be written to a px file.
#'
#' @param metadata Dataframe with metadata
#' @param data_cube Dataframe with data cube
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

#' Save data set to temporary file
#'
#' Create a temporary .rds data file that can be used by other functions in the
#' current R session.
#'
#' @param df Data frame to save.
#'
#' @return Path to temporary data
save_temp_data <- function(df) {
  temp_data_path <- paste0(tempfile(), '.rds')

  saveRDS(df, temp_data_path)

  return(temp_data_path)
}

#' Get the name of figures variable
#'
#' @param metadata_path Path to metadata
#'
#' @returns Character
get_figures_variable <- function(metadata_path) {
  figures_var <-
    metadata_path %>%
    get_variables_metadata() %>%
    dplyr::filter(tolower(type) == "figures") %>%
    dplyr::distinct(variable) %>%
    dplyr::pull(variable)

  error_if_not_exactly_one_figures_variable(figures_var)

  return(figures_var)
}

#' Wrapper aroud add_totals() to get values arguments from metadata
#'
#' @param metadata_path Path to metadata
#' @param source_data_path Path to source data
#' @param add_totals List of variables to add totals to.
add_totals_to_source_data <- function(metadata_path,
                                      source_data_path,
                                      add_totals) {
  variables <-
    metadata_path %>%
    get_variables_metadata()

  codelist <-
    metadata_path %>%
    get_codelist_metadata() %>%
    dplyr::select(variable, code, value)

  params <-
    variables %>%
    dplyr::select(variable, language, elimination) %>%
    dplyr::left_join(codelist, by = c("variable", "elimination" = "value")) %>%
    dplyr::filter(variable %in% add_totals, language == "en") %>%
    dplyr::distinct(variable, code)

  source_data_path <-
    add_totals(get_source_data(source_data_path, metadata_path),
               vars = params$variable,
               level_names = params$code,
               sum_var = get_figures_variable(metadata_path)
    ) %>%
    save_temp_data()

  return(source_data_path)
}

#' Create pxfile
#'
#' `pxmake()` creates a px file by combine source data from a `.rds` file and
#' metadata from an Excel workbook.
#'
#' @param metadata_path Path to Excel workbook with metadata.
#' @param pxfile_path Path to save px file at.
#' @param source_data_path Path to `.rds` file with data source, if left blank
#' `pxmake()` uses the data the metadata sheet 'Data'.
#' @param add_totals A list of variables to add a 'total' level to. The value
#' of the total level is looked up in 'Variables' xx_elimination. The code for
#' the level is found in 'Codelists'. The total is a sum of the values in the
#' variables with type = FIGURES in 'Variables'.
#' @return Nothing.
#'
#' @export
pxmake <- function(metadata_path,
                   pxfile_path,
                   source_data_path = NULL,
                   add_totals = NULL) {
  if (is.null(source_data_path)) {
    error_if_excel_sheet_does_not_exist("Data", metadata_path)

    source_data_path <-
      metadata_path %>%
      readxl::read_excel(sheet = "Data") %>%
      save_temp_data()
  }

  if (!is.null(add_totals)) {
    source_data_path <- add_totals_to_source_data(metadata_path,
                                                  source_data_path,
                                                  add_totals)
  }

  metadata  <- get_metadata(metadata_path, source_data_path)
  data_cube <- get_data_cube(metadata_path, source_data_path)

  px_lines <- format_px_data_as_lines(metadata, data_cube)

  file_connection <- file(pxfile_path)
  writeLines(px_lines, file_connection)
  close(file_connection)
}
