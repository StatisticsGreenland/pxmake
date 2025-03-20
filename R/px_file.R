#' Get data cube used in PX-file format
#'
#' @inheritParams sort_metadata_df
#' @inheritParams format_data_df
#'
#' @returns A data frame
#' @keywords internal
get_data_cube <- function(metadata_df, data_df) {
  metadata_df <- add_main_language(metadata_df)

  metadata_df_main_language <-
    metadata_df %>%
    dplyr::filter(.data$main_language)

  labels <-
    metadata_df_main_language %>%
    dplyr::filter(.data$keyword == "VARIABLECODE") %>%
    tidyr::unnest("value") %>%
    dplyr::select("label" = "variable", "variable" = "value")

  stub_and_heading_df <-
    metadata_df_main_language %>%
    dplyr::filter(.data$keyword %in% c("STUB", "HEADING")) %>%
    tidyr::unnest("value") %>%
    dplyr::select("keyword", "label" = "value") %>%
    dplyr::left_join(labels, by = "label") %>%
    dplyr::mutate(variable = ifelse(is.na(.data$variable),
                                    .data$label,
                                    .data$variable
                                    )
                  )

  stub_vars <-
    stub_and_heading_df %>%
    dplyr::filter(.data$keyword == "STUB") %>%
    dplyr::pull(.data$variable) %>%
    intersect(names(data_df))

  heading_vars <-
    stub_and_heading_df %>%
    dplyr::filter(.data$keyword == "HEADING") %>%
    dplyr::pull(.data$variable) %>%
    intersect(names(data_df))

  head_stub_variable_names <- c(heading_vars, stub_vars)

  figures_var <- setdiff(names(data_df),  head_stub_variable_names)

  cells <-
    metadata_df %>%
    dplyr::filter(.data$keyword == "CODES", .data$main_language) %>%
    dplyr::select("keyword", "variable", "value") %>%
    tidyr::unnest("value") %>%
    dplyr::rename("label" = "variable") %>%
    dplyr::group_by("label") %>%
    dplyr::mutate(sortorder = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(labels, by = "label") %>%
    dplyr::filter(.data$variable %in% names(data_df)) %>%
    dplyr::select("variable", "sortorder", "code" = "value")

  # Complete data by adding all combinations of variable values in data and
  # cells
  data_values <-
    data_df %>%
    mutate_all_vars_to_character() %>%
    dplyr::select(dplyr::all_of(head_stub_variable_names)) %>%
    lst_distinct_and_arrange()

  cells_values <-
    split(cells$code, cells$variable) %>%
    lst_distinct_and_arrange()

  data_and_cells_values <- merge_named_lists(data_values, cells_values)

  heading_code_vars      <- paste0("code_",      heading_vars, recycle0 = TRUE)
  heading_sortorder_vars <- paste0("sortorder_", heading_vars, recycle0 = TRUE)
  stub_code_vars         <- paste0("code_",      stub_vars,    recycle0 = TRUE)
  stub_sortorder_vars    <- paste0("sortorder_", stub_vars,    recycle0 = TRUE)

  data_cube <-
    data_df %>%
    mutate_all_vars_to_character() %>%
    tidyr::complete(!!!data_and_cells_values) %>%
    dplyr::mutate(id_ = dplyr::row_number()) %>% # used to unpivot data later
    tidyr::pivot_longer(cols = all_of(head_stub_variable_names),
                        names_to = "variable",
                        values_to = "code"
                        ) %>%
    dplyr::left_join(cells, by = c("variable", "code")) %>%
    tidyr::pivot_wider(names_from = "variable",
                       values_from = c("code", "sortorder")
                       ) %>%
    dplyr::select(-"id_") %>%
    # Sort by sortorder for first heading var, codes for first heading var,
    # sortorder for second heading var, etc.
    dplyr::arrange(dplyr::across(zip_vectors(heading_sortorder_vars, heading_code_vars))) %>%
    dplyr::select(-all_of(heading_sortorder_vars)) %>%
    { if(length(heading_code_vars > 0)) {
      tidyr::pivot_wider(.,
                         names_from = all_of(heading_code_vars),
                         values_from = all_of(figures_var)
                         )
    } else {
      .
    }
    } %>%
    dplyr::arrange(dplyr::across(zip_vectors(stub_sortorder_vars, stub_code_vars))) %>%
    dplyr::select(-ends_with(paste0("_", stub_vars)))

  return(data_cube)
}

#' Get lines for PX-file from px object
#'
#' @param x A px object
#'
#' @returns A character vector
#' @keywords internal
format_px_object_as_lines <- function(x) {
  metadata_df <- get_metadata_df_from_px(x)

  time_variables <-
    metadata_df %>%
    dplyr::filter(.data$keyword == "TIMEVAL") %>%
    dplyr::pull(.data$variable)

  metadata_lines <-
    metadata_df %>%
    dplyr::left_join(dplyr::select(px_keywords, "keyword", "quote_value"),
                     by = 'keyword'
                     ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(keyword = .data$keyword %>%
                    add_language_to_keyword(get_main_language(metadata_df),
                                            .data$language
                                            ) %>%
                    add_sub_key_to_keyword(.data$variable) %>%
                    add_cell_to_keyword(.data$cell)
                  ) %>%
    dplyr::mutate(value = .data$value %>%
                    tidyr::replace_na("") %>%
                    paste(collapse = '","') %>%
                    ifelse(.data$quote_value,
                           quote_unless_yes_no(.),
                           .
                           ) %>%
                    break_long_lines(max_line_length = 256) %>%
                    list()
                  ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest("value") %>%
    dplyr::mutate(repeated_keyword = !is.na(dplyr::lag(.data$keyword)) &
                    .data$keyword == dplyr::lag(.data$keyword),
                  last = is.na(dplyr::lead(.data$repeated_keyword)) |
                    !dplyr::lead(.data$repeated_keyword)
                  ) %>%
    dplyr::mutate(line = stringr::str_c(ifelse(.data$repeated_keyword,
                                                     "",
                                                     paste0(.data$keyword, "=")
                                                     ),
                                              .data$value,
                                              ifelse(.data$last, ";", "")
                                              )
                  ) %>%
    dplyr::pull(.data$line)

  data_lines <-
    get_data_cube(metadata_df, x$data) %>%
    mutate_all_vars_to_character() %>%
    dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(.x, '"-"'))) %>%
    tidyr::unite("tmp", sep = " ") %>%
    dplyr::pull(.data$tmp)

  c(metadata_lines, "DATA=", data_lines, ";")
}

#' Save px object to PX-file
#'
#' @param x A px object
#' @param path Path to save PX-file at
#'
#' @returns Nothing
#' @keywords internal
save_px_as_px_file <- function(x, path) {
  px_lines <- format_px_object_as_lines(x)

  encoding_str <-
    x$table1 %>%
    dplyr::filter(.data$keyword == "CODEPAGE") %>%
    dplyr::pull("value")

  if (length(encoding_str) == 0) {
    encoding_str <- get_default_encoding()
  }

  file_connection <- file(path, encoding = encoding_str)
  writeLines(px_lines, file_connection)
  close(file_connection)
}

#' Create a px object form a PX-file
#'
#' @param path Path to a PX-file
#'
#' @returns A px object
#' @keywords internal
px_from_px_file <- function(path) {
  px_lines <- read_px_file(path)

  data_line_index <- stringr::str_which(px_lines, '^DATA=$')

  error_if_not_exactly_one_data_line(data_line_index)

  metadata_lines <- px_lines[c(1:data_line_index)]
  data_lines     <- px_lines[c((data_line_index+1):length(px_lines))]

  metadata_df <-
    metadata_lines %>%
    get_metadata_df_from_px_lines() %>%
    add_main_language()

  variable_label <- get_variable_label(metadata_df)

  stub_heading_variables <-
    variable_label %>%
    dplyr::filter(.data$main_language, .data$keyword %in% c("HEADING", "STUB")) %>%
    dplyr::arrange(desc(.data$keyword), .data$index) %>%
    dplyr::pull("variable-code")

  name_relation <-
    variable_label %>%
    dplyr::distinct(.data$`variable-code`,
                    .data$language,
                    .data$`variable-label`,
                    .data$main_language
                    )

  metadata <-
    metadata_df %>%
    dplyr::rename("variable-label" = "variable") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`variable-label` = ifelse(.data$keyword %in% ("CONTVARIABLE"),
                                            unlist(.data$value),
                                            .data$`variable-label`
                                            )
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::select(name_relation, -"main_language"),
                     by = c("language", "variable-label")
                     ) %>%
    dplyr::left_join(px_keywords, by = "keyword")

  # languages
  languages <-
    metadata %>%
    dplyr::filter(.data$keyword == "LANGUAGES") %>%
    tidyr::unnest("value") %>%
    dplyr::select("language" = "value") %>%
    align_data_frames(get_base_languages())

  table_sheet_data <-
    metadata %>%
    dplyr::filter(.data$table_meta) %>%
    # Exclude variable specific NOTE
    dplyr::filter(!(.data$keyword == "NOTE" & !is.na(.data$`variable-label`))) %>%
    tidyr::unnest("value")

  # table1
  table1 <-
    table_sheet_data %>%
    dplyr::filter(! .data$language_dependent) %>%
    dplyr::select("keyword", "value") %>%
    align_data_frames(get_base_table1()) %>%
    sort_table1()

  # table2
  table2 <-
    table_sheet_data %>%
    dplyr::filter(.data$language_dependent) %>%
    dplyr::filter(.data$keyword != "CONTVARIABLE") %>% # CONTVAR is controlled in variables1
    dplyr::select('keyword', "code" = "cell", "language", "value") %>%
    align_data_frames(get_base_table2()) %>%
    sort_table2(languages = languages$language)

  # variable1
  figures_variable <-
    variable_label %>%
    dplyr::filter(is.na(.data$keyword)) %>%
    dplyr::distinct(.data$`variable-code`) %>%
    dplyr::pull("variable-code")

  if (identical(figures_variable, character(0))) {
    figures_variable <- "figures_"

    name_relation <-
      name_relation %>%
      dplyr::bind_rows(tidyr::crossing("variable-code" = figures_variable,
                                       "variable-label" = figures_variable,
                                       language = unique(name_relation$language)
                                       )
                       )
    }

  time_var <-
    metadata %>%
    dplyr::filter(.data$keyword == "TIMEVAL", .data$main_language) %>%
    dplyr::pull("variable-code")

  timeval <-
    metadata %>%
    dplyr::filter(.data$keyword == "TIMEVAL", .data$main_language) %>%
    tidyr::unnest("value") %>%
    dplyr::pull("variable-code")

  timeval_df <-
    name_relation %>%
    dplyr::filter(.data$main_language) %>%
    dplyr::select("variable-code") %>%
    dplyr::mutate(timeval = ifelse(.data$`variable-code` %in% timeval,
                                   TRUE,
                                   FALSE
                                   )
                  )

  contvariable <-
    metadata %>%
    dplyr::filter(.data$keyword == "CONTVARIABLE", .data$main_language) %>%
    tidyr::unnest("value") %>%
    dplyr::pull("variable-code")

  contvariable_df <-
    name_relation %>%
    dplyr::filter(.data$main_language) %>%
    dplyr::select("variable-code") %>%
    dplyr::mutate(contvariable = ifelse(.data$`variable-code` %in% contvariable,
                                        TRUE,
                                        FALSE
                                        )
                  )

  variable_type_df <-
    metadata %>%
    dplyr::filter(.data$main_language, .data$keyword %in% c("VARIABLE-TYPE")) %>%
    tidyr::unnest("value") %>%
    dplyr::filter(! toupper(.data$value) %in% c("TIMEVAL", "CONTVARIABLE")) %>%
    dplyr::select("variable-code", "variable-type" = "value")

  variables1 <-
    variable_label %>%
    dplyr::distinct(.data$`variable-code`,
                    "pivot" = .data$keyword,
                    "order" = .data$index
                    ) %>%
    dplyr::left_join(variable_type_df, by = "variable-code") %>%
    dplyr::left_join(contvariable_df, by = "variable-code") %>%
    dplyr::left_join(timeval_df, by = "variable-code") %>%
    dplyr::filter(! .data$`variable-code` %in% figures_variable) %>%
    dplyr::bind_rows(dplyr::tibble(`variable-code` = figures_variable,
                                   pivot = "FIGURES",
                                   contvariable = FALSE,
                                   timeval = FALSE
                                   )
                     ) %>%
    align_data_frames(get_base_variables1()) %>%
    sort_variables1()

  # variables2
  variables2 <-
    metadata %>%
    dplyr::filter(.data$keyword %in% c("NOTE", "ELIMINATION", "DOMAIN"),
                  ! is.na(.data$`variable-code`)
                  ) %>%
    dplyr::mutate(keyword = tolower(.data$keyword)) %>%
    dplyr::select("keyword", "language", "variable-code",
                  "variable-label", "value"
                  ) %>%
    tidyr::unnest("value") %>%
    tidyr::pivot_wider(names_from = "keyword",
                       values_from = "value"
                       ) %>%
    # Add variables without NOTE, ELIMINATION, DOMAIN to get all variable-labels
    dplyr::bind_rows(dplyr::anti_join(dplyr::select(name_relation, -"main_language"),
                                      .,
                                      by=c("variable-code",
                                           "language",
                                           "variable-label"
                                           )
                                      )
                     ) %>%
    align_data_frames(get_base_variables2()) %>%
    sort_variables2(data_table_names =  stub_heading_variables,
                    languages = languages$language
                    )

  # cells1, cells2
  codes <-
    metadata %>%
    dplyr::filter(.data$main_language, .data$keyword %in% c("CODES")) %>%
    tidyr::unnest("value") %>%
    dplyr::rename("code" = "value") %>%
    dplyr::group_by(.data$`variable-code`) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select("variable-code", "code", "order")

  precision <-
    metadata %>%
    dplyr::filter(.data$keyword == "PRECISION") %>%
    tidyr::unnest("value") %>%
    dplyr::rename("value" = "cell",
                  "precision" = "value"
                  ) %>%
    dplyr::select("variable-code", "value", "precision")

  values <-
    metadata %>%
    dplyr::filter(.data$keyword %in% c("VALUES")) %>%
    tidyr::unnest("value") %>%
    dplyr::group_by(.data$`variable-code`, .data$language) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::mutate(`variable-code` = ifelse(is.na(.data$`variable-code`),
                                           .data$`variable-label`,
                                           .data$`variable-code`
                                           )
                  ) %>%
    dplyr::select("variable-code", "value", "language", "main_language", "order")

  valuenote <-
    metadata %>%
    dplyr::filter(.data$keyword %in% c("VALUENOTE")) %>%
    tidyr::unnest("value") %>%
    dplyr::select("variable-code", "language", "value" = "cell",
                  "valuenote" = "value",
                  "language"
                  )

  codes_and_values <-
    codes %>%
    # Use main language values as codes, if codes are missing
    dplyr::full_join(values, by = c("variable-code", "order"),
                     multiple = "all"
                     ) %>%
    dplyr::full_join(valuenote, by = c("variable-code", "value", "language")) %>%
    dplyr::group_by(.data$`variable-code`, .data$order) %>%
    dplyr::mutate(code = ifelse(is.na(.data$code),
                                .data$value[.data$main_language],
                                .data$code
                                )
                  ) %>%
    dplyr::ungroup()

  cells <-
    codes_and_values %>%
    dplyr::select(-"main_language") %>%
    dplyr::left_join(precision, by = c("variable-code", "value"))

  cells1 <-
    cells %>%
    dplyr::distinct(.data$`variable-code`, .data$code, .data$order, .data$precision) %>%
    align_data_frames(get_base_cells1()) %>%
    sort_cells1(data_table_names = stub_heading_variables)

  cells2 <-
    cells %>%
    dplyr::select("variable-code", "code", "language", "value", "valuenote") %>%
    align_data_frames(get_base_cells2()) %>%
    sort_cells2(data_table_names = stub_heading_variables,
                languages = languages$language
                )

  # acrosscells
  acrosscells_variables <- intersect(unique(metadata$keyword),
                                    c("CELLNOTE", "CELLNOTEX")
                                    )

  acrosscells <-
    metadata %>%
    dplyr::filter(.data$keyword %in% acrosscells_variables) %>%
    dplyr::mutate(keyword = tolower(.data$keyword)) %>%
    tidyr::pivot_wider(names_from = "keyword", values_from = "value") %>%
    dplyr::mutate(`variable-label` = stringr::str_remove_all(.data$`variable-label`, '"')) %>%
    tidyr::separate_wider_delim(cols = "variable-label",
                                delim = ',',
                                names = stub_heading_variables
                                ) %>%
    dplyr::select(all_of(c(stub_heading_variables,
                           "language",
                           tolower(acrosscells_variables)
                           )
                         )
                  ) %>%
    align_data_frames(get_base_acrosscells(stub_heading_variables)) %>%
    dplyr::mutate(across(tolower(acrosscells_variables), ~ dplyr::na_if(.x, "NULL")))

  # data
  # Order: stub1, stub2, ..., heading1, heading2, ...
  expand_order <-
    variable_label %>%
    dplyr::filter(.data$main_language) %>%
    dplyr::mutate(keyword_order = dplyr::case_when(.data$keyword == "STUB" ~ 1,
                                                   .data$keyword == "HEADING" ~ 2,
                                                   TRUE ~ NA
                                                   )
                  ) %>%
    dplyr::arrange(.data$keyword_order, .data$index) %>%
    dplyr::mutate(expand_order = dplyr::row_number()) %>%
    dplyr::select("variable-code", "expand_order")

  if (identical(time_var, character(0))) {
    time_values_df <- tidyr::tibble()
  } else {
    time_values <-
      metadata %>%
      dplyr::filter(.data$keyword %in% c("TIMEVAL"), .data$main_language) %>%
      tidyr::unnest("value") %>%
      dplyr::pull("value") %>%
      get_values_from_time_format()

    time_values_df <- dplyr::tibble("variable-code" = time_var,
                                    "code" = time_values
                                    )
  }

  stub_and_heading_values <-
    codes_and_values %>%
    dplyr::filter(.data$main_language,
                  .data$`variable-code` %in% stub_heading_variables,
                  # Exclude time variable as it is already in time_values_df
                  ! .data$`variable-code` %in% time_var
                  ) %>%
    dplyr::bind_rows(time_values_df) %>%
    dplyr::distinct(.data$`variable-code`, .data$code) %>%
    dplyr::group_by(.data$`variable-code`) %>%
    dplyr::summarise(code = list(.data$code)) %>%
    dplyr::left_join(expand_order, by = "variable-code") %>%
    dplyr::arrange(.data$expand_order) %>%
    dplyr::select("variable-code", "code") %>%
    tibble::deframe()

  figures <-
    data_lines %>%
    stringr::str_replace_all(";", "") %>%
    stringr::str_split(" ") %>%
    unlist() %>%
    stringr::str_subset("^$", negate = TRUE) %>%
    tibble::enframe(name = NULL, value = figures_variable) %>%
    dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

  data_df <-
    do.call(tidyr::expand_grid, stub_and_heading_values) %>%
    dplyr::bind_cols(figures) %>%
    dplyr::as_tibble()

  new_px(languages = languages,
         table1 = table1,
         table2 = table2,
         variables1 = variables1,
         variables2 = variables2,
         cells1 = cells1,
         cells2 = cells2,
         acrosscells = acrosscells,
         data = data_df
         )
}
