#' Create a px object
#'
#' Create a px object from a px file, an Excel metadata workbook, or a data
#' frame.
#'
#' @param input Path to px file, path to an Excel metadata workbook, a data
#' frame or path to an .rds file with a data frame. If input is a data frame, a
#' px object with minimal metadata is created.
#' @param data Either a data frame or a path to an `.rds` file with a data frame.
#' This can only be used if `input` is an Excel metadata workbook. If NULL, the
#' data should be provided in the 'Data' sheet of the Excel workbook.
#'
#' @return A px object
#'
#' @export
px <- function(input, data = NULL) {
  validate_px_arguments(input, data)

  if (is_rds_file(data)) {
    data <- readRDS(data)
  }

  if (is_px_file(input)) {
    px <- px_from_px_file(input)
  } else if (is_xlsx_file(input)) {
    px <- px_from_excel(input, data)
  } else if (is.data.frame(input)) {
    px <- px_from_data_df(input)
  } else {
    unexpected_error()
  }

  validate_px(px)
}

#' Save px object to file
#'
#' @param x A px object.
#' @param path Path to file. The file extension determines the format. Can be:
#' - `.px` to save as a px file
#' - `.xlsx` to save as an Excel metadata workbook
#'
#' @return Nothing
#' @export
pxsave <- function(x, path) {
  validate_pxsave_arguments(x, path)

  if (is_px_file(path)) {
    save_px_as_px_file(x, path)
  } else if (is_xlsx_file(path)) {
    save_px_as_xlsx(x, path)
  } else {
    unexpected_error()
  }
}

#' Create new px object
#'
#' px constructor for internal functions
#'
#' @param languages A data frame with language metadata.
#' @param table1 A data frame with language independent table metadata.
#' @param table2 A data frame with language dependent table metadata.
#' @param variables1 A data frame with language independent variable metadata.
#' @param variables2 A data frame with language dependent variable metadata.
#' @param codelists1 A data frame with language independent codelist metadata.
#' @param codelists2 A data frame with language dependent codelist metadata.
#' @param data A data frame with data.
#'
#' @return A px object
new_px <- function(languages, table1, table2, variables1, variables2,
                   codelists1, codelists2, data) {
  x <- list(languages  = dplyr::as_tibble(languages),
            table1     = dplyr::as_tibble(table1),
            table2     = dplyr::as_tibble(table2),
            variables1 = dplyr::as_tibble(variables1),
            variables2 = dplyr::as_tibble(variables2),
            codelists1 = dplyr::as_tibble(codelists1),
            codelists2 = dplyr::as_tibble(codelists2),
            data       = dplyr::as_tibble(data)
            )

  structure(x, class = "px")
}

#' Validate px object
#'
#' Throws an error if the px object is not valid.
#'
#' @param x A supposed px object.
#'
#' @return A valid px object.
validate_px <- function(x) {
  if (! is.list(x)) {
    stop("px object must be a list", call. = FALSE)
  }

  if (! inherits(x, "px")) {
    stop("px object must have class 'px'", call. = FALSE)
  }

  px_target <- get_base_px()

  input_names  <- names(x)
  target_names <- names(px_target)

  missing_names <- setdiff(target_names, input_names)
  invalid_names <- setdiff(input_names, target_names)

  if (length(missing_names) > 0) {
    stop("px object is missing these names: ",
         paste0(missing_names, collapse = ", "),
         call. = FALSE
         )
  }

  if (length(invalid_names) > 0) {
    stop("px object contains invalid names: ",
         paste0(invalid_names, collapse = ", "),
         call. = FALSE
         )
  }

  for (name in input_names) {
    if (! is.data.frame(x[[name]])) {
      stop("px object element '", name, "' must be a data frame", call. = FALSE)
    }
  }

  for (name in input_names) {
    missing_names <- setdiff(names(px_target[[name]]), names(x[[name]]))

    if (length(missing_names) > 0) {
      stop("px object is missing these names in element '", name, "': ",
           paste0(missing_names, collapse = ", "),
           call. = FALSE
           )
    }
  }

  if (length(timeval(x)) > 1) {
    stop("px object has more than one time variable: ",
         paste0(timeval(x), collapse = ", "),
         call. = FALSE
         )
  }

  languages_in_tables <-
    unique(c(x$table2$language,
             x$variables2$language,
             x$codelists2$language)
           ) %>% na.omit() %>% as.character()

  defined_languages <- unique(c(language(x), languages(x)))

  undefined_languages <- setdiff(languages_in_tables, defined_languages)

  if (length(undefined_languages) > 0) {
    stop("px object contains languages that are not defined in 'x$languages' ",
         "or as keyword 'LANGUAGES' in x$table1: ",
         paste0(undefined_languages, collapse = ", "),
         call. = FALSE
         )
  }

  if (length(languages(x)) > 0) {
    if (! any(is.null(language(x)), language(x) %in% languages(x))) {
      stop("px object: LANGUAGE is not in x$languages. ", call. = FALSE)
    }
  }

  if (any(is.na(x$variables2$`variable-label`))) {
    stop("px object: in x$variables2 variable-label cannot be NA.",
         call. = FALSE)
  }

  return(x)
}
