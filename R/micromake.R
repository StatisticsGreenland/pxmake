create_micro_file <- function(micro_var, x, filenames, keyword_values_long, out_dir) {
  figures_var <- "n"

  new_data <-
    x$data %>%
    dplyr::select(all_of(c(px_heading(x), micro_var))) %>%
    dplyr::count(across(everything()), name = figures_var) %>%
    dplyr::arrange_all() %>%
    format_data_df(figures_variable = figures_var)

  headings_with_non_na_values <-
    new_data %>%
    dplyr::filter(!!rlang::sym(micro_var) != "-") %>%
    dplyr::select(all_of(px_heading(x))) %>%
    dplyr::distinct_all()

  headings_with_only_na_values <-
    new_data %>%
    dplyr::select(all_of(px_heading(x))) %>%
    {if (length(px_heading(x)) > 0)
      dplyr::anti_join(., headings_with_non_na_values, by = px_heading(x))
     else dplyr::filter(headings_with_non_na_values, FALSE)
    }

  if (nrow(headings_with_non_na_values) == 0) {
    # Edge case: If all headings only contain NA values, all are kept
    headings_with_only_na_values <- headings_with_non_na_values
  }

  if (length(px_heading(x)) > 0) {
    # Remove headings where the figures variable only has NA values
    new_data <-
      new_data %>%
      dplyr::anti_join(headings_with_only_na_values, by = px_heading(x))
  }

  data_names <- names(new_data)

  headings_with_only_na_values_long <-
    headings_with_only_na_values %>%
    {if (nrow(.) > 0)
      tidyr::pivot_longer(.,
                          cols = px_heading(x),
                          names_to = "variable-code",
                          values_to = "code"
                          )
    else dplyr::tibble(`variable-code` = character(), code = character())
    }

  x_new <-
    x %>%
    px_stub(micro_var)

  x_micro <-
    new_px(languages  = x_new$languages,
           table1     = x_new$table1,
           table2     = x_new$table2,
           variables1 = dplyr::filter(x_new$variables1, `variable-code` %in% data_names),
           variables2 = dplyr::filter(x_new$variables2, `variable-code` %in% data_names),
           cells1 = dplyr::filter(x_new$cells1, `variable-code` %in% data_names) %>%
                          dplyr::anti_join(headings_with_only_na_values_long,
                                           by = c("variable-code", "code")
                                           ),
           cells2 = dplyr::filter(x_new$cells2, `variable-code` %in% data_names) %>%
                          dplyr::anti_join(headings_with_only_na_values_long,
                                           by = c("variable-code", "code")
                                           ),
           acrosscells = dplyr::select(x_new$acrosscells,
                                      all_of(c(setdiff(data_names, figures_var),
                                               names(get_base_acrosscells())
                                               )
                                             )
                                      ) %>%
                          tidyr::drop_na({{micro_var}}),
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
#' The main loop uses the furrr package for parallelisation. Use future::plan()
#' to choose how to parallelise.
#'
#' @param x A px object.
#' @param out_dir Directory to save px files in.
#' @param keyword_values Optional. A data frame with column 'variable' and one
#' or more of: 'px_contents', 'px_title', 'px_description', and 'px_matrix'. The
#' columns will be added as keywords to the table for each non-HEADING variable
#' that match the 'variable' column. It probably work for other keywords as well.
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

  micro_vars <- setdiff(names(x$data), px_heading(x))

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

  furrr::future_walk(micro_vars,
                     create_micro_file,
                     x = x,
                     filenames = filenames,
                     keyword_values_long = keyword_values_long,
                     out_dir = out_dir
                     )

  if (print_out_dir) print(paste("Created px files in:", out_dir))
}
