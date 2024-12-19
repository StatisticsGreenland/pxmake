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

  x_micro <-
    new_px(languages  = x$languages,
           table1     = x$table1,
           table2     = x$table2,
           variables1 = dplyr::filter(x$variables1, .data$`variable-code` %in% data_names),
           variables2 = dplyr::filter(x$variables2, .data$`variable-code` %in% data_names),
           cells1 = dplyr::filter(x$cells1, .data$`variable-code` %in% data_names) %>%
                          dplyr::anti_join(headings_with_only_na_values_long,
                                           by = c("variable-code", "code")
                                           ),
           cells2 = dplyr::filter(x$cells2, .data$`variable-code` %in% data_names) %>%
                          dplyr::anti_join(headings_with_only_na_values_long,
                                           by = c("variable-code", "code")
                                           ),
           acrosscells = dplyr::select(x$acrosscells,
                                      all_of(c(setdiff(data_names, figures_var),
                                               names(get_base_acrosscells())
                                               )
                                             )
                                      ) %>%
                          tidyr::drop_na({{micro_var}}),
           data       = new_data
           ) %>%
    fix_px() %>%
    px_figures(figures_var, validate = FALSE)

  if (all(! is.null(keyword_values_long), nrow(keyword_values_long) > 0)) {
    extra_keywords <-
      keyword_values_long %>%
      dplyr::filter(.data$variable %in% micro_var)

    keyword_functions <- unique(extra_keywords$keyword_function)

    for (fnc in keyword_functions) {
      language_dependent_keyword <-
        function_to_keyword(fnc) %in% language_dependant_keywords()

      value <-
        extra_keywords %>%
        dplyr::filter(.data$keyword_function == fnc) %>%
        { if (language_dependent_keyword & "language" %in% names(.)) {
          dplyr::distinct(., .data$language, .data$value)
        } else {
          dplyr::distinct(., value) %>%
            dplyr::pull(value)
        }}

      modifying_function <- get(fnc)

      x_micro <- modifying_function(x = x_micro, value = value)
    }
  }

  if (any(is.null(filenames[micro_var]), is.na(filenames[micro_var]))) {
    filename <- paste0(micro_var, ".px")
  } else {
    filename <- filenames[micro_var]
  }

  px_save(x = x_micro, path = file.path(out_dir, filename))
}

#' Create micro PX-files
#'
#' Split one px object into many small PX-files (micro files), with count of
#' the variables in it.
#'
#' The HEADING variables are use in all the micro files, and a file is created
#' for each non-HEADING variable. The new PX-files are saved in a directory
#' specified by `out_dir`.
#'
#' The main loop uses the furrr package for parallelisation. Use future::plan()
#' to choose how to parallelise.
#'
#' @param x A px object.
#' @param out_dir Directory to save PX-files in.
#' @param keyword_values Optional. A data frame with column 'variable' and one
#' or more of: 'px_contents', 'px_title', 'px_description', and 'px_matrix'. The
#' columns will be added as keywords to the table for each non-HEADING variable
#' that match the 'variable' column. It probably work for other keywords as well.
#'
#' Use the column 'filename' to control the filename of each micro file. The
#' filename path is relative to 'out_dir'.
#'
#' Use the column 'language' if the PX-file has multiple languages.
#'
#' @returns Nothing
#'
#' @examples
#' # Create px object with cohort as HEADING
#' x <-
#'   greenlanders |>
#'   px() |>
#'   px_stub(names(greenlanders)) |>
#'   px_heading("cohort")
#'
#' # Create micro files, one for each of the non-HEADING variables (gender, age,
#' # municipality)
#' px_micro(x)
#'
#' @export
px_micro <- function(x, out_dir = NULL, keyword_values = NULL) {
  validate_px_micro_arguments(x, out_dir)

  print_out_dir <- is.null(out_dir)

  if (is.null(out_dir)) out_dir <- temp_dir()

  micro_vars <- setdiff(names(x$data), px_heading(x))

  if (! is.null(keyword_values)) {
    if ("filename" %in% colnames(keyword_values)) {
      filenames <-
        keyword_values %>%
        dplyr::select("variable", "filename") %>%
        tibble::deframe()
    } else {
      filenames <- NULL
    }

    keyword_values_long <-
      keyword_values %>%
      tidyr::pivot_longer(cols = setdiff(names(.), c("variable", "language")),
                          names_to = "keyword_function"
                          ) %>%
      tidyr::drop_na("value") %>%
      dplyr::filter(.data$keyword_function != "filename")
  } else {
    keyword_values_long <- NULL
    filenames <- NULL
  }

  furrr::future_walk(micro_vars,
                     create_micro_file,
                     x = px_stub(x, micro_vars, validate = FALSE),
                     filenames = filenames,
                     keyword_values_long = keyword_values_long,
                     out_dir = out_dir
                     )

  if (print_out_dir) print(paste("Created PX-files in:", out_dir))
}
