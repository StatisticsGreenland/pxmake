create_micro_file <- function(micro_var, x, filenames, keyword_values_long, out_dir) {
  figures_var <- "n"

  new_data <-
    x$data %>%
    dplyr::select(all_of(c(heading(x), micro_var))) %>%
    dplyr::count(across(everything()), name = figures_var) %>%
    dplyr::arrange_all() %>%
    format_data_df(figures_variable = figures_var)

  data_names <- names(new_data)

  x_new <-
    x %>%
    stub(micro_var)

  x_micro <-
    new_px(languages  = x_new$languages,
           table1     = x_new$table1,
           table2     = x_new$table2,
           variables1 = dplyr::filter(x_new$variables1, `variable-code` %in% data_names),
           variables2 = dplyr::filter(x_new$variables2, `variable-code` %in% data_names),
           codelists1 = dplyr::filter(x_new$codelists1, `variable-code` %in% data_names),
           codelists2 = dplyr::filter(x_new$codelists2, `variable-code` %in% data_names),
           data       = new_data
           ) %>%
    fix_px() %>%
    figures(figures_var)

  if (all(! is.null(keyword_values_long), nrow(keyword_values_long) > 0)) {
    extra_keywords <-
      keyword_values_long %>%
      dplyr::filter(variable %in% micro_var)

    for (i in 1:nrow(extra_keywords)) {
      modifying_function <- get(extra_keywords$keyword_function[i])

      x_micro <- modifying_function(x = x_micro, value = extra_keywords$value[i])
    }
  }

  if (any(is.null(filenames[micro_var]), is.na(filenames[micro_var]))) {
    filename <- paste0(micro_var, ".px")
  } else {
    filename <- filenames[micro_var]
  }

  pxsave(x = x_micro, path = file.path(out_dir, filename))
}

#' Create micro px files
#'
#' Split one px object into many small px files (micro files), with count of
#' the variables in it.
#'
#' The HEADING variables are use in all the micro files, and a file is created
#' for each non-HEADING variable. The new px files are saved in a directory
#' specified by `out_dir`.
#'
#' @param x A px object.
#' @param out_dir Directory to save px files in.
#' @param keyword_values Optional. A data frame with column 'variable' and one
#' or more of: 'contents', 'title', 'description', and 'matrix'. The columns
#' will be added as keywords to the table for each non-HEADING variable that
#' match the 'variable' column. It probably work for other keywords as well.
#'
#' Use the column 'filename' to control the filename of each micro file. The
#' filename path is relative to 'out_dir'.
#'
#' Currently doesn't support multiple languages.
#'
#' @return Nothing
#' @export
micromake <- function(x, out_dir = NULL, keyword_values = NULL) {
  validate_micromake_arguments(x, out_dir)

  print_out_dir <- is.null(out_dir)

  if (is.null(out_dir)) out_dir <- temp_dir()

  micro_vars <- setdiff(names(x$data), heading(x))

  if (! is.null(keyword_values)) {
    if ("filename" %in% colnames(keyword_values)) {
      filenames <-
        dplyr::select(keyword_values, variable, filename) %>%
        tibble::deframe()
    } else {
      filenames <- NULL
    }

    keyword_values_long <-
      keyword_values %>%
      tidyr::pivot_longer(cols = setdiff(names(.), c("variable", "language")),
                          names_to = "keyword_function"
                          ) %>%
      dplyr::filter(keyword_function != "filename")
  } else {
    keyword_values_long <- NULL
    filenames <- NULL
  }

  # for (micro_var in micro_vars) {
  #   create_micro_file(x, micro_var, filenames, keyword_values_long, out_dir)
  # }

  furrr::future_walk(micro_vars,
                     create_micro_file,
                     x = x,
                     filenames = filenames,
                     keyword_values_long = keyword_values_long,
                     out_dir = out_dir)

  if (print_out_dir) print(paste("Created px files in:", out_dir))
}
