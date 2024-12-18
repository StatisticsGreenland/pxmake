#' Get languages used in px object
#'
#' @param x A px object
#'
#' @returns A character vector
#' @keywords internal
defined_languages <- function(x) {
  if (! is.null(px_languages(x))) {
    return(px_languages(x))
  } else if (! is.null(px_language(x))) {
    return(px_language(x))
  } else {
    return(NA_character_)
  }
}

#' Change language in table
#'
#' @param df A data frame to change languages in
#' @param new_languages A character vector
#' @param keep_vars A character vector
#' @param align_df A data frame to align with
#'
#' @returns A data frame
#' @keywords internal
modify_languages_in_table <- function(df, new_languages, keep_vars, align_df) {
  add_languages <- setdiff(new_languages, df$language)

  keep <- dplyr::filter(df, .data$language %in% new_languages)

  new  <-
    df %>%
    dplyr::select(all_of(keep_vars)) %>%
    tidyr::expand_grid(language = add_languages) %>%
    align_data_frames(align_df)

  dplyr::bind_rows(keep, new)
}

#' Change language in px object
#'
#' Changes languages in 3 of the px tables
#'
#' @param x A px object
#' @param new_languages A character vector
#'
#' @returns A px object
#' @keywords internal
modify_languages_in_px <- function(x, new_languages) {
  x$table2 <-
    modify_languages_in_table(df = x$table2,
                              new_languages = new_languages,
                              keep_vars = c("keyword", "value"),
                              align_df = get_base_table2()
                              )

  x$variables2 <-
    modify_languages_in_table(df = x$variables2,
                              new_languages = new_languages,
                              keep_vars = c("variable-code", "variable-label"),
                              align_df = get_base_variables2()
                              )

  x$cells2 <-
    modify_languages_in_table(df = x$cells2,
                              new_languages = new_languages,
                              keep_vars = c("variable-code", "code"),
                              align_df = get_base_cells2()
                              )
  x$acrosscells <-
    modify_languages_in_table(df = x$acrosscells,
                              new_languages = new_languages,
                              keep_vars = c(px_stub(x), px_heading(x), 'cellnote'),
                              align_df = get_base_acrosscells()
                              )

  return(x)
}

#' @rdname px_language.px
#' @export
px_language <- function(x, value, validate) {
  UseMethod("px_language")
}

#' LANGUAGE
#'
#' Inspect or change LANGUAGE.
#'
#' If LANGUAGES is defined, changing LANGUAGE will also add is to LANGUAGES.
#'
#' @param x A px object
#' @param value Optional. A character string. If missing, the current LANGUAGE
#' is returned. If NULL, LANGUAGE is removed.
#' @eval param_validate()
#'
#' @seealso \code{\link{px_languages}}
#'
#' @export
px_language.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    language <- get_table1_value(x, "LANGUAGE")

    if (identical(language, character(0))) {
      return(NULL)
    } else {
      return(language)
    }
  } else if (is.null(value)) {
    return(remove_keyword_table1(x, "LANGUAGE"))
  }

  if (!is.null(px_languages(x)) & !value %in% px_languages(x)) {
    x$languages <- modify_or_add_in_column(x$languages, "language", px_language(x), value)
  }

  x <- modify_table1(x, "LANGUAGE", value)

  x <- modify_languages_in_px(x, new_languages = defined_languages(x))

  return_px(x, validate)
}


#' @rdname px_languages.px
#' @export
px_languages <- function(x, value, validate) {
  UseMethod("px_languages")
}

#' LANGUAGES
#'
#' Inspect or change LANGUAGES.
#'
#' If LANGUAGE is defined it should be one of the values in LANGUAGES.
#'
#' @param x A px object
#' @param value Optional. A character vector. If missing, the current LANGUAGES
#' are returned. If NULL, LANGUAGES are removed.
#' @eval param_validate()
#'
#' @seealso \code{\link{px_language}}
#'
#' @export
px_languages.px <- function(x, value, validate = TRUE) {
  if (missing(value)) {
    languages <- x$languages$language

    if (identical(languages, character(0))) {
      return(NULL)
    } else {
      return(languages)
    }
  } else if (is.null(value)) {
    x$languages <- get_base_languages()
    return(x)
  }

  x$languages <-
    dplyr::tibble(language = value) %>%
    align_data_frames(get_base_languages())

  x <- modify_languages_in_px(x, new_languages = value)

  return_px(x, validate)
}
