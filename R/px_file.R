#' Get data cube used in PX-file format
#'
#' @inheritParams sort_metadata_df
#' @inheritParams format_data_df
#'
#' @returns A data frame
#' @keywords internal
get_data_cube <- function(metadata_df, data_df) {
  data_df <- materialize_px_data(data_df)
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
    dplyr::mutate(
      variable = ifelse(
        is.na(.data$variable),
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

  figures_var <- setdiff(names(data_df), head_stub_variable_names)

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
    dplyr::mutate(id_ = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = all_of(head_stub_variable_names),
      names_to = "variable",
      values_to = "code"
    ) %>%
    dplyr::left_join(cells, by = c("variable", "code")) %>%
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = c("code", "sortorder")
    ) %>%
    dplyr::select(-"id_") %>%
    dplyr::arrange(dplyr::across(zip_vectors(heading_sortorder_vars, heading_code_vars))) %>%
    dplyr::select(-all_of(heading_sortorder_vars)) %>%
    {
      if (length(heading_code_vars) > 0) {
        tidyr::pivot_wider(
          .,
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

#' Write compact DATA lines directly to a connection
#'
#' @param x A px object with compact data
#' @param metadata_df Metadata data frame from px object
#' @param con A writable connection
#'
#' @returns Nothing
#' @keywords internal
write_compact_data_lines <- function(x, metadata_df, con) {
  stopifnot(is_px_data_compact(x$data))

  metadata_df <- add_main_language(metadata_df)

  labels <-
    metadata_df %>%
    dplyr::filter(.data$main_language, .data$keyword == "VARIABLECODE") %>%
    tidyr::unnest("value") %>%
    dplyr::select("label" = "variable", "variable" = "value")

  heading_vars <-
    metadata_df %>%
    dplyr::filter(.data$main_language, .data$keyword == "HEADING") %>%
    tidyr::unnest("value") %>%
    dplyr::select("label" = "value") %>%
    dplyr::left_join(labels, by = "label") %>%
    dplyr::mutate(
      variable = ifelse(is.na(.data$variable), .data$label, .data$variable)
    ) %>%
    dplyr::pull(.data$variable) %>%
    intersect(names(x$data$dimensions))

  line_width <- prod(lengths(x$data$dimensions[heading_vars]))
  if (length(line_width) == 0 || is.na(line_width) || line_width < 1) {
    line_width <- 1L
  }

  vals <- ifelse(is.na(x$data$figures), '"-"', as.character(x$data$figures))
  n_vals <- length(vals)
  n_lines <- ceiling(n_vals / line_width)

  large <- is_large_px_dataset(n_vals)
  prog <- new_px_progress(total = 100, enabled = large)
  on.exit(if (!is.null(prog)) prog$close(), add = TRUE)

  chunk_lines <- 1000L

  for (chunk_start in seq(1L, n_lines, by = chunk_lines)) {
    chunk_end <- min(chunk_start + chunk_lines - 1L, n_lines)
    out <- character(chunk_end - chunk_start + 1L)

    j <- 1L
    for (i in chunk_start:chunk_end) {
      start <- ((i - 1L) * line_width) + 1L
      end <- min(i * line_width, n_vals)
      out[[j]] <- paste(vals[start:end], collapse = " ")
      j <- j + 1L
    }

    writeLines(out, con)

    if (large) {
      tick_px_progress_10(prog, chunk_end, n_lines, prefix = "Writing data values")
    }
  }

  writeLines(";", con)
}

#' Read DATA values with progress in 10% steps for large datasets
#'
#' @param data_lines Character vector with PX DATA lines
#'
#' @returns Character vector of values
#' @keywords internal
read_px_data_values <- function(data_lines) {
  if (length(data_lines) > 0) {
    data_lines[length(data_lines)] <- sub(";\\s*$", "", data_lines[length(data_lines)])
  }

  n_lines <- length(data_lines)
  large <- is_large_px_dataset(n_lines)
  prog <- new_px_progress(total = 100, enabled = large)
  on.exit(if (!is.null(prog)) prog$close(), add = TRUE)

  out <- vector("list", n_lines)

  for (i in seq_len(n_lines)) {
    vals <- scan(
      text = data_lines[[i]],
      what = character(),
      quiet = TRUE
    )

    out[[i]] <- vals

    if (large) {
      tick_px_progress_10(prog, i, n_lines, prefix = "Reading data values")
    }
  }

  unlist(out, use.names = FALSE)
}

#' Format compact PX data as DATA lines
#'
#' @param x A px object with compact data
#' @param metadata_df Metadata data frame from px object
#'
#' @returns A character vector of DATA lines
#' @keywords internal
format_compact_data_as_lines <- function(x, metadata_df) {
  stopifnot(is_px_data_compact(x$data))

  metadata_df <- add_main_language(metadata_df)

  metadata_df_main_language <-
    metadata_df %>%
    dplyr::filter(.data$main_language)

  labels <-
    metadata_df_main_language %>%
    dplyr::filter(.data$keyword == "VARIABLECODE") %>%
    tidyr::unnest("value") %>%
    dplyr::select("label" = "variable", "variable" = "value")

  heading_vars <-
    metadata_df_main_language %>%
    dplyr::filter(.data$keyword == "HEADING") %>%
    tidyr::unnest("value") %>%
    dplyr::select("label" = "value") %>%
    dplyr::left_join(labels, by = "label") %>%
    dplyr::mutate(
      variable = ifelse(is.na(.data$variable), .data$label, .data$variable)
    ) %>%
    dplyr::pull(.data$variable) %>%
    intersect(names(x$data$dimensions))

  line_width <- prod(lengths(x$data$dimensions[heading_vars]))

  if (length(line_width) == 0 || is.na(line_width) || line_width < 1) {
    line_width <- 1L
  }

  vals <- ifelse(is.na(x$data$figures), '"-"', as.character(x$data$figures))
  n_vals <- length(vals)

  large <- is_large_px_dataset(n_vals)
  prog <- new_px_progress(total = 100, enabled = large)
  on.exit(if (!is.null(prog)) prog$close(), add = TRUE)

  n_lines <- ceiling(n_vals / line_width)
  out <- character(n_lines)

  for (i in seq_len(n_lines)) {
    start <- ((i - 1L) * line_width) + 1L
    end <- min(i * line_width, n_vals)
    out[[i]] <- paste(vals[start:end], collapse = " ")

    if (large) {
      tick_px_progress_10(prog, i, n_lines, prefix = "Writing data values")
    }
  }

  out
}

#' Get lines for PX-file from px object
#'
#' @param x A px object
#'
#' @returns A character vector
#' @keywords internal
format_px_object_as_lines <- function(x) {
  metadata_df <- get_metadata_df_from_px(x)

  metadata_lines <-
    metadata_df %>%
    dplyr::left_join(
      dplyr::select(pxmake::px_keywords, "keyword", "quote_value"),
      by = "keyword"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      keyword = .data$keyword %>%
        add_language_to_keyword(get_main_language(metadata_df), .data$language) %>%
        add_sub_key_to_keyword(.data$variable) %>%
        add_cell_to_keyword(.data$cell)
    ) %>%
    dplyr::mutate(
      value = .data$value %>%
        tidyr::replace_na("") %>%
        paste(collapse = '","') %>%
        ifelse(.data$quote_value, quote_unless_yes_no(.), .) %>%
        break_long_lines(max_line_length = 256) %>%
        list()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest("value") %>%
    dplyr::mutate(
      repeated_keyword = !is.na(dplyr::lag(.data$keyword)) &
        .data$keyword == dplyr::lag(.data$keyword),
      last = is.na(dplyr::lead(.data$repeated_keyword)) |
        !dplyr::lead(.data$repeated_keyword)
    ) %>%
    dplyr::mutate(
      line = stringr::str_c(
        ifelse(.data$repeated_keyword, "", paste0(.data$keyword, "=")),
        .data$value,
        ifelse(.data$last, ";", "")
      )
    ) %>%
    dplyr::pull(.data$line)

  data_lines <- if (is_px_data_compact(x$data)) {
    format_compact_data_as_lines(x, metadata_df)
  } else {
    get_data_cube(metadata_df = metadata_df, data_df = x$data) %>%
      mutate_all_vars_to_character() %>%
      dplyr::mutate(dplyr::across(everything(), ~ tidyr::replace_na(.x, '"-"'))) %>%
      tidyr::unite("tmp", sep = " ") %>%
      dplyr::pull(.data$tmp)
  }

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
  tmp_path <- paste0(path, ".tmp")

  if (file.exists(tmp_path)) {
    unlink(tmp_path)
  }

  encoding_str <-
    x$table1 %>%
    dplyr::filter(.data$keyword == "CODEPAGE") %>%
    dplyr::pull("value")

  if (length(encoding_str) == 0) {
    encoding_str <- get_default_encoding()
  }

  if (!is_px_data_compact(x$data)) {
    px_lines <- format_px_object_as_lines(x)

    file_connection <- file(tmp_path, open = "wt", encoding = encoding_str)
    writeLines(px_lines, file_connection)
    close(file_connection)

    if (file.exists(path)) {
      unlink(path)
    }

    ok <- file.rename(tmp_path, path)
    if (!ok) {
      error(paste0("Could not rename temporary file '", tmp_path, "' to '", path, "'."))
    }

    return(invisible(NULL))
  }

  metadata_df <- get_metadata_df_from_px(x)

  metadata_lines <-
    metadata_df %>%
    dplyr::left_join(
      dplyr::select(pxmake::px_keywords, "keyword", "quote_value"),
      by = "keyword"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      keyword = .data$keyword %>%
        add_language_to_keyword(get_main_language(metadata_df), .data$language) %>%
        add_sub_key_to_keyword(.data$variable) %>%
        add_cell_to_keyword(.data$cell)
    ) %>%
    dplyr::mutate(
      value = .data$value %>%
        tidyr::replace_na("") %>%
        paste(collapse = '","') %>%
        ifelse(.data$quote_value, quote_unless_yes_no(.), .) %>%
        break_long_lines(max_line_length = 256) %>%
        list()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest("value") %>%
    dplyr::mutate(
      repeated_keyword = !is.na(dplyr::lag(.data$keyword)) &
        .data$keyword == dplyr::lag(.data$keyword),
      last = is.na(dplyr::lead(.data$repeated_keyword)) |
        !dplyr::lead(.data$repeated_keyword)
    ) %>%
    dplyr::mutate(
      line = stringr::str_c(
        ifelse(.data$repeated_keyword, "", paste0(.data$keyword, "=")),
        .data$value,
        ifelse(.data$last, ";", "")
      )
    ) %>%
    dplyr::pull(.data$line)

  file_connection <- file(tmp_path, open = "wt", encoding = encoding_str)
  writeLines(metadata_lines, file_connection)
  writeLines("DATA=", file_connection)
  write_compact_data_lines(x, metadata_df, file_connection)
  close(file_connection)

  if (file.exists(path)) {
    unlink(path)
  }

  ok <- file.rename(tmp_path, path)
  if (!ok) {
    error(paste0("Could not rename temporary file '", tmp_path, "' to '", path, "'."))
  }

  invisible(NULL)
}

#' Create a px object form a PX-file
#'
#' @param path Path to a PX-file
#'
#' @returns A px object
#' @keywords internal
px_from_px_file <- function(path) {
  prog <- new_px_progress(total = 4)
  on.exit(if (!is.null(prog)) prog$close(), add = TRUE)

  if (!is.null(prog)) prog$tick(1, "Reading PX file")
  px_lines <- read_px_file(path)

  data_line_index <- stringr::str_which(px_lines, '^DATA=$')
  error_if_not_exactly_one_data_line(data_line_index)

  metadata_lines <- px_lines[1:data_line_index]
  data_lines     <- px_lines[(data_line_index + 1):length(px_lines)]

  if (!is.null(prog)) prog$tick(2, "Parsing metadata")
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
    dplyr::distinct(
      .data$`variable-code`,
      .data$language,
      .data$`variable-label`,
      .data$main_language
    )

  metadata <-
    metadata_df %>%
    dplyr::rename("variable-label" = "variable") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      `variable-label` = ifelse(
        .data$keyword %in% "CONTVARIABLE",
        unlist(.data$value),
        .data$`variable-label`
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      dplyr::select(name_relation, -"main_language"),
      by = c("language", "variable-label")
    ) %>%
    dplyr::left_join(pxmake::px_keywords, by = "keyword")

  languages <-
    metadata %>%
    dplyr::filter(.data$keyword == "LANGUAGES") %>%
    tidyr::unnest("value") %>%
    dplyr::select("language" = "value") %>%
    align_data_frames(get_base_languages())

  table_sheet_data <-
    metadata %>%
    dplyr::filter(.data$table_meta) %>%
    dplyr::filter(!(.data$keyword == "NOTE" & !is.na(.data$`variable-label`))) %>%
    tidyr::unnest("value")

  table1 <-
    table_sheet_data %>%
    dplyr::filter(!.data$language_dependent) %>%
    dplyr::select("keyword", "value") %>%
    align_data_frames(get_base_table1()) %>%
    sort_table1()

  table2 <-
    table_sheet_data %>%
    dplyr::filter(.data$language_dependent) %>%
    dplyr::filter(.data$keyword != "CONTVARIABLE") %>%
    dplyr::select("keyword", "code" = "cell", "language", "value") %>%
    align_data_frames(get_base_table2()) %>%
    sort_table2(languages = languages$language)

  figures_variable <-
    variable_label %>%
    dplyr::filter(is.na(.data$keyword)) %>%
    dplyr::distinct(.data$`variable-code`) %>%
    dplyr::pull("variable-code")

  if (identical(figures_variable, character(0))) {
    figures_variable <- "figures_"

    name_relation <-
      name_relation %>%
      dplyr::bind_rows(
        tidyr::crossing(
          "variable-code" = figures_variable,
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
    dplyr::mutate(timeval = ifelse(.data$`variable-code` %in% timeval, TRUE, FALSE))

  contvariable <-
    metadata %>%
    dplyr::filter(.data$keyword == "CONTVARIABLE", .data$main_language) %>%
    tidyr::unnest("value") %>%
    dplyr::pull("variable-code")

  contvariable_df <-
    name_relation %>%
    dplyr::filter(.data$main_language) %>%
    dplyr::select("variable-code") %>%
    dplyr::mutate(contvariable = ifelse(.data$`variable-code` %in% contvariable, TRUE, FALSE))

  variable_type_df <-
    metadata %>%
    dplyr::filter(.data$main_language, .data$keyword %in% c("VARIABLE-TYPE")) %>%
    tidyr::unnest("value") %>%
    dplyr::filter(!toupper(.data$value) %in% c("TIMEVAL", "CONTVARIABLE")) %>%
    dplyr::select("variable-code", "variable-type" = "value")

  variables2 <-
    metadata %>%
    dplyr::filter(.data$keyword %in% c("NOTE", "DOMAIN", "MAP"), !is.na(.data$`variable-code`)) %>%
    dplyr::mutate(keyword = tolower(.data$keyword)) %>%
    dplyr::select("keyword", "language", "variable-code", "variable-label", "value") %>%
    tidyr::unnest("value") %>%
    tidyr::pivot_wider(names_from = "keyword", values_from = "value") %>%
    dplyr::bind_rows(
      dplyr::anti_join(
        dplyr::select(name_relation, -"main_language"),
        .,
        by = c("variable-code", "language", "variable-label")
      )
    ) %>%
    align_data_frames(get_base_variables2()) %>%
    sort_variables2(data_table_names = stub_heading_variables, languages = languages$language)

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
    dplyr::rename("value" = "cell", "precision" = "value") %>%
    dplyr::select("variable-code", "value", "precision")

  values <-
    metadata %>%
    dplyr::filter(.data$keyword %in% c("VALUES")) %>%
    tidyr::unnest("value") %>%
    dplyr::group_by(.data$`variable-code`, .data$language) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::mutate(
      `variable-code` = ifelse(
        is.na(.data$`variable-code`),
        .data$`variable-label`,
        .data$`variable-code`
      )
    ) %>%
    dplyr::select("variable-code", "value", "language", "main_language", "order")

  valuenote <-
    metadata %>%
    dplyr::filter(.data$keyword %in% c("VALUENOTE")) %>%
    tidyr::unnest("value") %>%
    dplyr::select("variable-code", "language", "value" = "cell", "valuenote" = "value", "language")

  codes_and_values <-
    codes %>%
    dplyr::full_join(values, by = c("variable-code", "order"), multiple = "all") %>%
    dplyr::full_join(valuenote, by = c("variable-code", "value", "language")) %>%
    dplyr::group_by(.data$`variable-code`, .data$order) %>%
    dplyr::mutate(
      code = ifelse(
        is.na(.data$code),
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
    sort_cells2(data_table_names = stub_heading_variables, languages = languages$language)

  elimination_df <-
    metadata %>%
    dplyr::filter(.data$main_language, .data$keyword %in% c("ELIMINATION")) %>%
    tidyr::unnest("value") %>%
    dplyr::left_join(
      dplyr::filter(codes_and_values, .data$main_language),
      by = c("variable-code", "value")
    ) %>%
    dplyr::mutate(code = ifelse(is.na(.data$code), .data$value, .data$code)) %>%
    dplyr::select("variable-code", "elimination" = "code")

  variables1 <-
    variable_label %>%
    dplyr::distinct(.data$`variable-code`, "pivot" = .data$keyword, "order" = .data$index) %>%
    dplyr::left_join(variable_type_df, by = "variable-code") %>%
    dplyr::left_join(contvariable_df, by = "variable-code") %>%
    dplyr::left_join(timeval_df, by = "variable-code") %>%
    dplyr::left_join(elimination_df, by = "variable-code") %>%
    dplyr::filter(!.data$`variable-code` %in% figures_variable) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        `variable-code` = figures_variable,
        pivot = "FIGURES",
        contvariable = FALSE,
        timeval = FALSE
      )
    ) %>%
    align_data_frames(get_base_variables1()) %>%
    sort_variables1()

  acrosscells_variables <- intersect(unique(metadata$keyword), c("CELLNOTE", "CELLNOTEX"))

  acrosscells <-
    metadata %>%
    dplyr::filter(.data$keyword %in% acrosscells_variables) %>%
    dplyr::mutate(keyword = tolower(.data$keyword)) %>%
    tidyr::pivot_wider(names_from = "keyword", values_from = "value") %>%
    dplyr::mutate(`variable-label` = stringr::str_remove_all(.data$`variable-label`, '"')) %>%
    tidyr::separate_wider_delim(cols = "variable-label", delim = ",", names = stub_heading_variables) %>%
    dplyr::select(all_of(c(stub_heading_variables, "language", tolower(acrosscells_variables)))) %>%
    align_data_frames(get_base_acrosscells(stub_heading_variables)) %>%
    dplyr::mutate(across(tolower(acrosscells_variables), ~ dplyr::na_if(.x, "NULL")))

  expand_order <-
    variable_label %>%
    dplyr::filter(.data$main_language) %>%
    dplyr::mutate(
      keyword_order = dplyr::case_when(
        .data$keyword == "STUB" ~ 1,
        .data$keyword == "HEADING" ~ 2,
        TRUE ~ NA_real_
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

    time_values_df <- dplyr::tibble("variable-code" = time_var, "code" = time_values)
  }

  stub_and_heading_values <-
    codes_and_values %>%
    dplyr::filter(
      .data$main_language,
      .data$`variable-code` %in% stub_heading_variables,
      !.data$`variable-code` %in% time_var
    ) %>%
    dplyr::bind_rows(time_values_df) %>%
    dplyr::distinct(.data$`variable-code`, .data$code) %>%
    dplyr::group_by(.data$`variable-code`) %>%
    dplyr::summarise(code = list(.data$code), .groups = "drop") %>%
    dplyr::left_join(expand_order, by = "variable-code") %>%
    dplyr::arrange(.data$expand_order) %>%
    dplyr::select("variable-code", "code") %>%
    tibble::deframe()

  if (!is.null(prog)) prog$tick(3, "Reading data values")
  fig_vec <- read_px_data_values(data_lines)

  figures <-
    tibble::tibble(
      !!figures_variable := suppressWarnings(as.numeric(fig_vec))
    )

  grid_n <- prod(lengths(stub_and_heading_values))

  if (should_materialize_px_data(grid_n)) {
    data_df <-
      do.call(tidyr::expand_grid, stub_and_heading_values) %>%
      dplyr::bind_cols(figures) %>%
      dplyr::as_tibble()
  } else {
    data_df <- new_px_data_compact(
      dimensions = stub_and_heading_values,
      figures = figures[[1]],
      figure_name = names(figures)[1]
    )
  }

  if (!is.null(prog)) prog$tick(4, "Building PX object")

  new_px(
    languages = languages,
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
