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
  merge_variables <- setdiff(names(df2), modify_column)

  replace_values <-
    dplyr::inner_join(dplyr::select(df1, -all_of(modify_column)),
                      df2, by = merge_variables
                      )

  keep_values <- dplyr::anti_join(df1, df2, by = merge_variables)

  add_values <- dplyr::anti_join(df2, df1, by = merge_variables)

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

modify_table1 <- function(x, keyword, value) {
  x$table1 <- modify_or_add_row(x$table1, "keyword", keyword, "value", value)
  return(x)
}

modify_table2 <- function(x, keyword, value) {
  if (is.data.frame(value)) {
    value$keyword <- keyword
    x$table2 <- modify_with_df(x$table2, value, "value")
  } else if (is.character(value)) {
    x$table2 <- modify_or_add_row(x$table2, "keyword", keyword, "value", value)
  }
   return(x)
}

modify_codelists2 <- function(x, column, value) {
  x$codelists2 <- modify_with_df(x$codelists2, value, column)
  return(x)
}

get_table1_value <- function(x, keyword) {
  x$table1 %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::pull(value)
}

get_table2_value <- function(x, keyword) {
  x$table2 %>%
    dplyr::filter(keyword == !!keyword) %>%
    dplyr::select(language, value)
}

get_codelists2_value <- function(x, column) {
  x$codelists2 %>%
    dplyr::select(`variable-code`, code, language, !!column) %>%
    tidyr::drop_na(!!column)
}
