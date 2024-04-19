#' Add or modify value
#'
#' Modify a value in a row of a data frame based on its value in another column.
#' If the value is not found, a new row is added.
#'
#' @param df Data frame
#' @param lookup_column Column to look up
#' @param lookup_column_values Values to look up
#' @param modify_column Column to modify
#' @param new_value New value to modify/add to modify_column
#'
#' @return A data frame
modify_or_add_row <- function(df,
                              lookup_column,
                              lookup_column_values,
                              modify_column,
                              new_value) {
  if (any(df[[lookup_column]] %in% lookup_column_values)) {
    df[df[[lookup_column]] %in% lookup_column_values, modify_column] <- new_value
  } else {
    df <- dplyr::bind_rows(df,
                           dplyr::tibble(!!lookup_column := lookup_column_values,
                                         !!modify_column := new_value
                           )
    )
  }

  return(df)
}

modify_or_add_in_column <- function(df,
                                    lookup_column,
                                    lookup_column_values,
                                    new_value
                                    ) {
  if (any(df[[lookup_column]] %in% lookup_column_values)) {
    df[df[[lookup_column]] %in% lookup_column_values, lookup_column] <- new_value
  } else {
    df <- dplyr::bind_rows(df,
                           dplyr::tibble(!!lookup_column := new_value)
                           )
  }

  return(df)
}

modify_with_df <- function(df1, df2, modify_column) {
  valid_names <-
    names(df1) %>%
    paste(collapse = ", ")

  invalid_columns <-
    setdiff(names(df2), names(df1)) %>%
    paste(collapse = ", ")

  if (invalid_columns != "") {
    error(stringr::str_glue(
      "Data frame contains invalid columns: ",
      "{invalid_columns}, it can only ",
      "contain the columns: {valid_names}"
    ))
  }

  merge_variables <- setdiff(names(df2), modify_column)

  df1_without_modify_column <- dplyr::select(df1, -all_of(modify_column))

  if (length(merge_variables) == 0) {
    replace_values <- dplyr::cross_join(df1_without_modify_column, df2)
    keep_values    <- dplyr::filter(df1, FALSE)
    add_values     <- dplyr::filter(df2, FALSE)
  } else {
    replace_values <- dplyr::inner_join(df1_without_modify_column, df2,
                                        by = merge_variables
                                        )
    keep_values <- dplyr::anti_join(df1, df2, by = merge_variables)
    add_values <- dplyr::anti_join(df2, df1, by = merge_variables)
  }

  return(dplyr::bind_rows(replace_values, keep_values, add_values))
}

remove_keyword_from_table <- function(table_name) {
  function(x, keyword) {
    x[[table_name]] <- x[[table_name]] %>%
      dplyr::filter(keyword != !!keyword)

    return(x)
  }
}

remove_keyword_table1 <- remove_keyword_from_table("table1")
remove_keyword_table2 <- remove_keyword_from_table("table2")

remove_keyword_variables2 <- function(x, keyword) {
  x$variables2[[tolower(keyword)]] <- NA
  return(x)
}

modify_table1 <- function(x, keyword, value) {
  x$table1 <- modify_or_add_row(x$table1, "keyword", keyword, "value", value)
  return(x)
}

modify_table2 <- function(x, keyword, value) {
  if (is.data.frame(value)) {
    value$keyword <- keyword
  } else if (is.character(value)) {
    value <-
      data.frame(keyword = keyword, value = value, language = defined_languages(x))
  }

  x$table2 <- modify_with_df(x$table2, value, "value")

  return(x)
}

modify_codelists1 <- function(x, column, value) {
  x$codelists1 <- modify_with_df(x$codelists1, value, column)
  return(x)
}

modify_codelists2 <- function(x, column, value) {
  x$codelists2 <- modify_with_df(x$codelists2, value, column)
  return(x)
}

modify_variables2 <- function(x, column, value) {
  if (is.character(value)) {
    value <- dplyr::tibble("{column}" := value)
  }

  x$variables2 <- modify_with_df(x$variables2, value, column)

  return(x)
}


get_table1_value <- function(x, keyword) {
  value <-
    x$table1 %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::pull(value)

  if (identical(value, character(0))) {
    return(NULL)
  } else {
    return(value)
  }
}

get_table2_value <- function(x, keyword) {
  value <-
    x$table2 %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::select(language, value)

  if (nrow(value) == 0) {
    return(NULL)
  } else if (all(length(defined_languages(x)) == 1, nrow(value) == 1)) {
    return(unique(value$value))
  } else {
    return(value)
  }
}

get_codelists1_value <- function(x, column) {
  x$codelists1 %>%
    dplyr::select(`variable-code`, code, !!column) %>%
    tidyr::drop_na(!!column)
}

get_codelists2_value <- function(x, column) {
  x$codelists2 %>%
    dplyr::select(`variable-code`, code, language, !!column) %>%
    tidyr::drop_na(!!column)
}

get_variables2_value <- function(x, column) {
  value <-
    x$variables2 %>%
    dplyr::select(`variable-code`, language, !!column) %>%
    tidyr::drop_na(!!column)

  if (nrow(value) == 0) {
    return(NULL)
  } else if (length(unique(x$variables2[[column]])) == 1) {
    return(unique(x$variables2[[column]]))
  } else {
    if (length(defined_languages(x)) == 1) {
      return(dplyr::select(value, -language))
    } else {
      return(value)
    }
  }
}
