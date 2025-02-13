dummy_data_path <- function() {
  'not_a_file'
}

#' Save px object as an R script
#'
#' Running the R script creates the px object.
#'
#' @param x A px object
#' @param path Path to save R script at
#'
#' @returns Nothing
#' @keywords internal
save_px_as_r_script <- function(x, path, data_path) {
  is_list_of_lists <- function(x) {
    all(purrr::map_lgl(x, is.list))
  }

  if (!is.null(data_path)) {
    if (is_rds_file(data_path)) {
      saveRDS(x$data, data_path)
    } else if (is_parquet_file(data_path)) {
      arrow::write_parquet(x$data, data_path)
    } else {
      unexpected_error()
    }
  } else {
    data_path <- dummy_data_path()
  }

  data_code <-
    px_keywords %>%
    # Remove unimplemented functions
    dplyr::filter(.data$px_function %in% getNamespaceExports('pxmake')) %>%
    # Add px_order
    dplyr::bind_rows(data.frame(keyword = NA_character_,
                                px_function = "px_order"
                                )
                     ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = list(eval(parse(text = paste0(.data$px_function, "(x)"))))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!purrr::map_lgl(.data$value, is.null)) %>%
    # Remove rows where value is default value
    dplyr::filter(!purrr::map2_lgl(.data$value, default_value, identical)) %>%
    # Expand values that are list of lists
    dplyr::mutate(value = purrr::map(.data$value, function(x) {
                                     if (is_list_of_lists(x)) x else list(x)
                                     })
                  ) %>%
    tidyr::unnest("value") %>%
    dplyr::mutate(value_constructor = purrr::map_chr(.data$value, convert_value_to_code)) %>%
    dplyr::select("keyword", "px_function", "value_constructor") %>%
    dplyr::arrange(.data$keyword != "DATA") %>%
    dplyr::mutate(last_row = dplyr::row_number() == dplyr::n()) %>%
    dplyr::mutate(
      code = dplyr::case_when(
        keyword == "DATA" & data_path == dummy_data_path() ~ stringr::str_glue("px(input = {.data$value_constructor}) %>%"),
        keyword == "DATA" & data_path != dummy_data_path() ~ stringr::str_glue('px(input = "{normalizePath(data_path, winslash = "/", mustWork = FALSE)}") %>%'),
        !last_row         ~ stringr::str_glue("  {.data$px_function}({.data$value_constructor}) %>%"),
        last_row          ~ stringr::str_glue("  {.data$px_function}({.data$value_constructor})")
        )
    ) %>%
    dplyr::pull(code) %>%
    paste(collapse = "\n")

  code <-
    c("library(dplyr)",
      "library(pxmake)",
      "",
      data_code
      ) %>%
    paste(collapse = "\n")

  writeLines(code, path)
}

#' Create constructing code
#'
#' Creates code that construct input value.
#'
#' @param value Vector of values to create constructors for
#'
#' @returns A character vector
#' @keywords internal
convert_value_to_code <- function(value) {
  if (is.data.frame(value)) {
    convert_df_to_code(value)
  } else if (is.character(value)) {
    if (length(value) == 1) {
      shQuote(value)
    } else {
      paste0("c(", paste0(shQuote(value), collapse = ", "), ")")
    }
  } else {
    unexpected_error()
  }
}

#' Create code to construct data frame
#'
#' Convert data frame to the code nessasary to construct it as a tibble.
#'
#' @param df A data frame
#'
#' @returns A character vector
#' @keywords internal
convert_df_to_code <- function(df) {
  col_names <-
    names(df) %>%
    purrr::map_chr(function(x) {
      if(make.names(x) == x) {
        x
      } else {
        paste0("`", x, "`")
      }
    }) %>%
    paste0("~", ., collapse = ", ")

  rows <-
    df %>%
    dplyr::mutate(across(where(~ is.factor(.) | is.character(.)),
                         ~ dplyr::if_else(is.na(.), "NA", shQuote(.))
                         ),
                  across(where(is.numeric),
                         ~ dplyr::if_else(is.na(.), "NA", as.character(.))
                         ),
                  ) %>%
    tidyr::unite("rows", everything(), sep = ", ") %>%
    dplyr::pull(1)

  c("tribble(",
    paste0("  ", col_names, ","),
    paste(" ", paste0(rows, ","), collapse = "\n"),
    "  )"
    ) %>%
  paste(collapse = "\n")
}


