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
#' @param table_keywords Optional. A data frame with columns 'variable' and
#' 'description'. The description is added to the table
#' for each non-HEADING variable. Currently doesn't support multiple languages.
#'
#' @return Nothing
#' @export
micromake <- function(x, out_dir = NULL, table_keywords = NULL) {
  validate_micromake_arguments(x, out_dir)

  print_out_dir <- is.null(out_dir)

  if (is.null(out_dir)) out_dir <- temp_dir()

  heading_variables <- heading(x)

  micro_vars <- setdiff(names(x$data), heading_variables)

  figures_var <- "n"

  new_px <-
    x %>%
    stub(micro_vars)

  if (! is.null(table_keywords)) {
    table_keywords_long <-
      table_keywords %>%
      tidyr::pivot_longer(cols = setdiff(names(.), c("variable", "language")),
                          names_to = "keyword_function"
                          )
  }

  for (micro_var in micro_vars) {
    new_data <-
      x$data %>%
      dplyr::select(all_of(c(heading_variables, micro_var))) %>%
      dplyr::count(across(everything()), name = figures_var) %>%
      dplyr::arrange_all() %>%
      format_data_df(figures_variable = figures_var)

    data_names <- names(new_data)

    x_micro <-
      new_px(languages  = new_px$languages,
           table1     = new_px$table1,
           table2     = new_px$table2,
           variables1 = dplyr::filter(new_px$variables1, `variable-code` %in% data_names),
           variables2 = dplyr::filter(new_px$variables2, `variable-code` %in% data_names),
           codelists1 = dplyr::filter(new_px$codelists1, `variable-code` %in% data_names),
           codelists2 = dplyr::filter(new_px$codelists2, `variable-code` %in% data_names),
           data       = new_data
           ) %>%
      fix_px() %>%
      figures(figures_var)

    if(!is.null(table_keywords)) {
      extra_keywords <-
        table_keywords_long %>%
        dplyr::filter(variable %in% micro_var)

      for (i in 1:nrow(extra_keywords)) {
        modifying_function <- get(extra_keywords$keyword_function[i])

        x_micro <- modifying_function(x = x_micro, value = extra_keywords$value[i])
      }
    }

    pxsave(x = x_micro,
           path = file.path(out_dir, paste0('micro_', micro_var, '.px'))
           )
  }

  if (print_out_dir) print(paste("Created px files in:", out_dir))
}
