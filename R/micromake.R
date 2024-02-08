#' Create microdata
#'
#' Turn a px object into many px files, one for each variable except time vars.
#'
#' @param px A px object.
#' @param out_dir Directory to save px files in.
#'
#' @returns Nothing
#' @export
micromake <- function(px, out_dir = NULL) {
  validate_micromake_arguments(px, out_dir)

  print_out_dir <- is.null(out_dir)

  if (is.null(out_dir)) out_dir <- temp_dir()

  time_var <- time_variable(px)

  micro_vars <- setdiff(names(px$data), c(time_var, figures_variable(px)))

  new_px <-
    px %>%
    figures("n") %>%
    stub(micro_vars) %>%
    { if (identical(time_var, character(0))) . else heading(., time_var)}

  for (micro_var in micro_vars) {
    new_data <-
      px$data %>%
      dplyr::select(all_of(c(time_var, micro_var))) %>%
      dplyr::count(across(everything())) %>%
      dplyr::arrange_all() %>%
      format_data_df(figures_variable = "n")

    data_names <- names(new_data)

    new_px(languages  = new_px$languages,
           table1     = new_px$table1,
           table2     = new_px$table2,
           variables1 = dplyr::filter(new_px$variables1, `variable-code` %in% data_names),
           variables2 = dplyr::filter(new_px$variables2, `variable-code` %in% data_names),
           codelists1 = dplyr::filter(new_px$codelists1, `variable-code` %in% data_names),
           codelists2 = dplyr::filter(new_px$codelists2, `variable-code` %in% data_names),
           data       = new_data
           ) %>%
      pxsave(path = file.path(out_dir, paste0('micro_', micro_var, '.px')))
  }

  if (print_out_dir) print(paste("Created px files in:", out_dir))
}
