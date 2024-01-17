#' Create px object
#'
#' Create a px object from a px-file, an Excel metadata workbook, or a list with
#' a specific structure.
#'
#' @param input Path to px-file, path to an Excel metadata workbook, or a list.
#' @param data Either a data frame or a path to an `.rds` file with a data frame.
#' This can only be used if the `input` argument is an Excel metadata workbook.
#' If NULL, the data should be provided in the 'data' sheet of the Excel
#' workbook.
#'
#' @return A px object invisibly.
#'
#' @export
px <- function(input, data = NULL) {
  validate_px_arguments(input, data)

  if (is_rds_file(data)) {
    data <- readRDS(data)
  }

  if (is.list(input)) {
    unexpected_error()
    #px <- validate_px(new_px(input))
  } else if (is_px_file(input)) {
    px <- px_from_px_file(input)
  } else if (is_xlsx_file(input)) {
    px <- px_from_excel(input, data)
  } else {
    unexpected_error()
  }

  validate_px(px)
}

#' Save px object to file
#'
#' @param px A px object.
#' @param path Path to file. The file extension determines the format. Can be:
#' - `.px` to save as a px-file
#' - `.xlsx` to save as an Excel metadata workbook
#'
#' @return Nothing
pxsave <- function(px, path) {
  validate_pxsave_arguments(px, path)

  if (is_px_file(path)) {
    save_px_as_px_file(px, path)
  } else if (is_xlsx_file(path)) {
    save_px_as_xlsx(px, path)
  } else {
    unexpected_error()
  }
}

pxtemplate <- function(data) {
  # same as data frame option in pxmake
  validate_px(new_px(x))
}

new_px <- function(languages, table1, table2, variables1, variables2,
                   codelists1, codelists2, data) {
  p <- list(languages = languages,
            table1 = table1,
            table2 = table2,
            variables1 = variables1,
            variables2 = variables2,
            codelists1 = codelists1,
            codelists2 = codelists2,
            data = data
            )

  structure(p, class = "px")
}

# Ideas for functions
# px(input, data=NA) (create a px object from an input file)
#
# pxtemplate(data) (create a px object from just a data frame)
# maybe pxtemplate is just px(input = data.frame, data = NA)
#
# pxsave(px, path) (save a px object to a path)
#
# add_totals(px, vars = list()) (add totals to a px object)
#
# bexsta <- px('path/to/px/file/bexsta.px')
# bexsta$languages



#' Validate px object
#'
#' @param x A supposed px object.
#'
#' @return A valid px object.
validate_px <- function(x) {
  if (! is.list(x)) {
    stop("px object must be a list", call. = FALSE)
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

  x
}
