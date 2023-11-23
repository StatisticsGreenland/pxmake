#' Create microdata
#'
#' Turn a dataset and its metadata into a series of px-files, one for each
#' variable in the original dataset, except time vars.
#'
#' @param data_df A data frame.
#' @param metadata_path An Excel workbook created by metamake.
#' @param out_dir Directory to save px-files in.
#'
#' @returns Nothing
#' @export
micromake <- function(data_df, metadata_path, out_dir = NULL) {
  print_out_dir <- is.null(out_dir)

  if (is.null(out_dir)) out_dir <- temp_dir()

  wb <- openxlsx::loadWorkbook(metadata_path)

  variables <- openxlsx::readWorkbook(wb, sheet="Variables") %>% dplyr::as_tibble()

  time_var <-
    variables %>%
    dplyr::filter(toupper(type) == "TIME") %>%
    dplyr::pull(`variable-code`)

  figures_var <-
    variables %>%
    dplyr::filter(toupper(pivot) == "FIGURES") %>%
    dplyr::pull(`variable-code`)

  micro_vars <- setdiff(names(data_df), c(time_var, figures_var))

  for (micro_var in micro_vars) {
    data_df_micro <-
      data_df %>%
      dplyr::select(all_of(c(time_var, micro_var))) %>%
      dplyr::count(across(everything())) %>%
      dplyr::arrange_all()

    pxmake(input = metadata_path,
           data = data_df_micro,
           out_path = file.path(out_dir, paste0('micro_', micro_var, '.px'))
           )
  }

  if (print_out_dir) print(paste("Created px-files in:", out_dir))
}
