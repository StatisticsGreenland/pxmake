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
micromake_old <- function(data_df, metadata_path, out_dir = NULL) {
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

#' Create microdata
#'
#' Turn a px object into many px-files, one for each variable except time vars.
#'
#' @param px A px object.
#' @param out_dir Directory to save px-files in.
#'
#' @returns Nothing
#' @export
micromake <- function(px, out_dir = NULL) {
  print_out_dir <- is.null(out_dir)

  if (is.null(out_dir)) out_dir <- temp_dir()

  time_var <-
    px$variables1 %>%
    dplyr::filter(toupper(type) == "TIME") %>%
    dplyr::pull(`variable-code`)

  figures_var <-
    px$variables1 %>%
    dplyr::filter(toupper(pivot) == "FIGURES") %>%
    dplyr::pull(`variable-code`)

  micro_vars <- setdiff(names(px$data), c(time_var, figures_var))

  for (micro_var in micro_vars) {
    print(micro_var)
    new_data <-
      data_df_micro <-
      px$data %>%
      dplyr::select(all_of(c(time_var, micro_var))) %>%
      dplyr::count(across(everything())) %>%
      dplyr::arrange_all()

    new_px(languages  = px$languages,
           table1     = px$table1,
           table2     = px$table2,
           variables1 = px$variables1,
           variables2 = px$variables2,
           codelists1 = px$codelists1,
           codelists2 = px$codelists2,
           data       = new_data
           ) %>%
      pxsave(path = file.path(out_dir, paste0('micro_', micro_var, '.px')))
  }

  if (print_out_dir) print(paste("Created px-files in:", out_dir))
}

