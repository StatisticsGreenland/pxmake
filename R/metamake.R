get_px_metadata_regex <- function() {
  paste0("(?<keyword>[[:upper:]-]+)",                    # Leading keyword
         "(?:\\[)?(?<language>[[:alpha:]_-]+)?(?:\\])?", # Maybe language
         "(?:\\()?(?<variable>[^\\(\\)]+)?(?:\\))?",     # Maybe sub-key
         "=",                                            # definitely =
         "(?<value>[^;]*)",                              # Up to ending ;
         "(?:;$)?"                                       # Maybe ;
  )
}

#' Create an Excel metadata workbook from a px-file
#'
#' Turn a px-file into an Excel metadata workbook. If pxmake() is run on that
#' workbook it turns back into the same px-file.
#'
#' @param px_file_path Path to px file
#' @param out_path Path to save file at
#'
#' @returns Nothing
#'
#' @export
metamake <- function(px_file_path, out_path) {
  lines <- readLines(px_file_path)

  ## Split metadata in data cube and heading
  data_line_index <- stringr::str_which(lines, '^DATA=$')

  if(length(data_line_index) != 1) {
    stop("unhandled error")
  }

  metadata_lines <- lines[c(1:data_line_index)]
  data_lines     <- lines[c((data_line_index+1):length(lines))]

  tmp_metadata <-
    metadata_lines %>%
    stringr::str_match(get_px_metadata_regex()) %>%
    magrittr::extract(,-1) %>% # remove full match column
    dplyr::as_tibble() %>%
    # remove leading and trailing "
    dplyr::mutate(dplyr::across(c(variable, value),
                                ~stringr::str_replace_all(., '^"|"$', '')),
                  value = stringr::str_split(value, '","')
                  )


  main_language <-
    tmp_metadata %>%
    dplyr::filter(keyword == "LANGUAGE") %>%
    dplyr::pull(value) %>%
    unlist()

  tmp_metadata2 <-
    tmp_metadata %>%
    dplyr::mutate(language = tidyr::replace_na(language, main_language))

  head_stub <-
    tmp_metadata2 %>%
    dplyr::filter(keyword %in% c("HEADING", "STUB")) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(long_name = value) %>%
    dplyr::group_by(keyword, language) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-variable)

  tmp <-
    head_stub %>%
    dplyr::filter(language == main_language) %>%
    dplyr::select(-language) %>%
    dplyr::rename(variable = long_name) %>%
    dplyr::right_join(head_stub,
                      by = dplyr::join_by(keyword, index),
                      multiple = "all"
                      )

  position <-
    tmp %>%
    dplyr::mutate(position = paste0(substr(keyword, 1, 1), index)) %>%
    dplyr::distinct(position, variable)

  name_relation <- tmp %>% dplyr::select(long_name, variable)

  metadata <-
    tmp_metadata2 %>%
    dplyr::rename(long_name = variable) %>%
    dplyr::left_join(name_relation, by = "long_name")

  ###
  ### Make variables sheet
  ###
  long_name <-
    metadata %>%
    dplyr::distinct(variable, language, long_name) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(names_from = language,
                       names_glue = "{language}_long_name",
                       values_from = long_name
    )

  note_elimination_domain <-
    metadata %>%
    dplyr::filter(keyword %in% c("NOTE", "ELIMINATION", "DOMAIN", "HEADING")) %>% #why heading?
    tidyr::drop_na(long_name) %>%
    dplyr::select(-long_name) %>%
    tidyr::unnest(value) %>%
    tidyr::pivot_wider(names_from = c(language, keyword),
                       names_glue = "{language}_{tolower(keyword)}",
                       values_from = value
                       )

  time_vars <-
    metadata %>%
    dplyr::filter(keyword == "TIMEVAL", language == main_language) %>%
    dplyr::pull(variable)

  sheet_variables <-
    position %>%
    dplyr::left_join(dplyr::tibble(variable = time_vars, type = "time"),
                     by = dplyr::join_by(variable)
                     ) %>%
    dplyr::left_join(long_name, by = dplyr::join_by(variable)) %>%
    dplyr::left_join(note_elimination_domain, by = dplyr::join_by(variable)) %>%
    dplyr::bind_rows(dplyr::tibble(variable = "value", type = "figures"))

  ###
  ### Make Codelists
  ###
  codes <-
    metadata %>%
    dplyr::filter(keyword %in% c("CODES"),
                  language == main_language,
                  !variable %in% time_vars #Time vars are not in codelist
                  ) %>%
    tidyr::unnest(value) %>%
    dplyr::rename(code = value) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(variable, code, sortorder)

  values <-
    metadata %>%
    dplyr::filter(keyword %in% c("VALUES")) %>%
    tidyr::unnest(value) %>%
    dplyr::group_by(variable, language) %>%
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::select(variable, value, language, sortorder)

  sheet_codelist <-
    codes %>%
    dplyr::right_join(values,
                      by = dplyr::join_by(variable, sortorder),
                      multiple = "all"
                      ) %>%
    tidyr::drop_na(code) %>%
    tidyr::pivot_wider(names_from = language, names_glue = "{language}_code_label") %>%
    dplyr::mutate(precision = "")

  ###
  ### Make General
  ###
  sheet_general <-
    metadata %>%
    dplyr::left_join(get_px_keywords(), by = "keyword") %>%
    dplyr::filter(metadata_sheet == "General") %>%
    # Exclude variable specific notes
    dplyr::filter(!(keyword == "NOTE" & !is.na(variable))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = paste(value, collapse = '","'),
                  keyword = ifelse(language_dependent,
                                   paste0(keyword, "_", language),
                                   keyword
                                   )
                  ) %>%
    dplyr::select(keyword, value)

  ###
  ### Make data
  ###
  heading_vars <-
    metadata %>%
    dplyr::filter(keyword == "HEADING", language == main_language) %>%
    dplyr::pull(value) %>%
    unlist()

  stub_vars <-
    metadata %>%
    dplyr::filter(keyword == "STUB", language == main_language) %>%
    dplyr::pull(value) %>%
    unlist()

  # Order: s1, s2, ..., h1, h2, ...
  expand_order <-
    head_stub %>%
    dplyr::filter(language == main_language) %>%
    dplyr::mutate(keyword_order = dplyr::case_when(keyword == "STUB" ~ 1,
                                                   keyword == "HEADING" ~ 2,
                                                   TRUE ~ NA
                                                   )
                  ) %>%
    dplyr::arrange(dplyr::across(c(keyword_order, index))) %>%
    dplyr::mutate(expand_order = dplyr::row_number()) %>%
    dplyr::select(long_name, expand_order)

  stub_and_heading_values <-
    metadata %>%
    dplyr::filter(keyword == "CODES", #should probably be changed to codes
                  language == main_language,
                  variable %in% c(heading_vars, stub_vars)
                  ) %>%
    dplyr::left_join(expand_order, by = "long_name") %>%
    dplyr::arrange(expand_order) %>%
    dplyr::select(variable, value) %>%
    tibble::deframe()

  value <-
    data_lines %>%
    stringr::str_replace_all(";", "") %>%
    stringr::str_split(" ") %>% unlist() %>% head(-1) %>%
    dplyr::tibble(value = .)

  sheet_data <-
    do.call(tidyr::expand_grid, stub_and_heading_values) %>%
    dplyr::bind_cols(value)

  ###
  ### Make workbook
  ###
  wb <- openxlsx::createWorkbook()

  add_sheet <- function(df, sheet_name) {
    openxlsx::addWorksheet(wb, sheetName = sheet_name)
    openxlsx::writeData(wb, sheet_name, df)
  }

  add_sheet(sheet_general,   "General")
  add_sheet(sheet_variables, "Variables")
  add_sheet(sheet_codelist,  "Codelists")
  add_sheet(sheet_data,  "Data")

  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
}
