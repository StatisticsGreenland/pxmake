#' @rdname px_data.px
#' @export
px_data <- function(x, value, validate) {
  UseMethod("px_data")
}

#' @eval add_doc_keyword_function_intro("DATA")
#' @param value Optional. A data frame. If missing, the current DATA is returned.
#' If NULL, all data rows are removed.
#' @eval add_return_px_or_df()
#' @eval param_validate()
#'
#' @details If adding a new data frame, metadata is generated for the new
#' columns and removed for columns that are no longer present.
#'
#' @examples
#' x1 <- px(population_gl)
#'
#' # Print data table
#' px_data(x1)
#'
#' # Change data table
#' population_gl_2024 <- subset(population_gl, year == 2024)
#'
#' x2 <- px_data(x1, population_gl_2024)
#'
#' @export
px_data.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    return(x$data)
  } else if (is.null(value)) {
    x$data <- dplyr::filter(x$data, FALSE)
  } else {
    old_df_columns <- names(px_data(x))
    df_columns <- names(value)
    new_columns <- setdiff(df_columns, old_df_columns)

    dummy_px <-
      px_from_data_df(value) %>%
      px_languages(defined_languages(x))

    if (! is.null(px_elimination(x))) {
      elimination_values <-
        px_values(x) %>%
        dplyr::semi_join(px_elimination(x),
                         by = c('variable-code' = 'variable-code',
                                'code' = 'elimination'
                                )
                         )

      dummy_px <- px_values(dummy_px, elimination_values)
    }

    # Add new variables from x2 to x1, and remove variables from x1 that are
    # not in x2.
    swap_in_variables_metadata <- function(x1, x2, element) {
      x1[[element]] <-
        x1[[element]] %>%
        dplyr::filter(.data$`variable-code` %in% df_columns) %>%
        dplyr::bind_rows(dplyr::filter(x2[[element]],
                                       .data$`variable-code` %in% new_columns
                                       )
                         )
      return(x1)
    }

    x <- swap_in_variables_metadata(x, dummy_px, element = 'variables1')
    x <- swap_in_variables_metadata(x, dummy_px, element = 'variables2')

    # Add new codes from x2 not in x1, and remove codes from x1 that are not
    # in x2.
    swap_in_cells_metadata <- function(x1, x2, element) {
      join_vars <- c("variable-code", "code")

      if (element == 'cells2') {
        join_vars <- c(join_vars, "language")
      }

      new_codes <-
        x2[[element]] %>%
        dplyr::anti_join(x1[[element]], by = join_vars)

      x1[[element]] <-
        x1[[element]] %>%
        dplyr::semi_join(x2[[element]], by = join_vars) %>%
        dplyr::bind_rows(new_codes)

      return(x1)
    }

    x <- swap_in_cells_metadata(x, dummy_px, element = 'cells1')
    x <- swap_in_cells_metadata(x, dummy_px, element = 'cells2')

    x$acrosscells <- get_base_acrosscells(c(px_stub(x), px_heading(x)))

    x$data <- value
  }

  return_px(x, validate)
}
