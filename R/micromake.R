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
  # Loop through all variables in data_df except the time variables
  # and create a px-file for each variable.
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



























#' #' Create microdata
#' #'
#' #' Turn a dataset and its metadata into a series of px-files, one for each
#' #' variable in the original dataset, except time vars.
#' #'
#' #' @inheritParams metamake
#' #' @param out_dir Directory to save px-files in.
#' #'
#' #' @returns Nothing
#' #' @export
#' micromake <- function(input, out_dir) {
#'   temp_excel <- temp_xlsx_file()
#'   temp_rds   <- temp_rds_file()
#'
#'   metamake(input, out_path = temp_excel, data_path = temp_rds)
#'
#'   wb <- openxlsx::loadWorkbook(temp_excel)
#'
#'   variables <- openxlsx::readWorkbook(wb, sheet="Variables") %>% dplyr::as_tibble()
#'
#'   variables_long <-
#'     variables %>%
#'     dplyr::mutate(across(-one_of("order"), as.character)) %>%
#'     tidyr::pivot_longer(cols = -c(pivot, order, `variable-code`, type),
#'                         names_to = c("language", "keyword"),
#'                         names_pattern = "^([[:alpha:]]+)_(.*)$"
#'                         ) %>%
#'     tidyr::pivot_wider(names_from = "keyword") %>%
#'     dplyr::bind_rows(dplyr::tibble(elimination = character())) # remove with 181
#'
#'   codelists <- openxlsx::readWorkbook(wb, sheet="Codelists")
#'
#'   codelists_long <-
#'     codelists %>%
#'     dplyr::as_tibble() %>%
#'     tidyr::pivot_longer(cols = ends_with("_code-label"),
#'                         names_to = c("language"),
#'                         names_pattern = "^([[:alpha:]]+)_.*$"
#'                         )
#'
#'   figures_var <-
#'     variables %>%
#'     dplyr::filter(toupper(pivot) == "FIGURES") %>%
#'     dplyr::pull(`variable-code`)
#'
#'   time_var <-
#'     variables %>%
#'     dplyr::filter(toupper(type) == "TIME") %>%
#'     dplyr::pull(`variable-code`)
#'
#'   stub_heading_vars <-
#'     variables %>%
#'     dplyr::filter(pivot %in% c("STUB", "HEADING")) %>%
#'     dplyr::pull(`variable-code`)
#'
#'   micro_vars <- setdiff(stub_heading_vars, time_var)
#'
#'   for (varname in micro_vars) {
#'     include_vars   <- c(varname, time_var, figures_var)
#'     eliminate_vars <- setdiff(micro_vars, varname)
#'
#'     temp_wb <- wb
#'
#'     overwrite_sheet_data <- function(df, workbook, sheet_name) {
#'       openxlsx::removeWorksheet(workbook, sheet_name)
#'       openxlsx::addWorksheet(workbook, sheet_name)
#'       openxlsx::writeDataTable(workbook, sheet_name, df)
#'     }
#'
#'     variables %>%
#'       dplyr::filter(`variable-code` %in% include_vars) %>%
#'       dplyr::mutate(pivot = dplyr::case_when(`variable-code` %in% time_var    ~ "HEADING",
#'                                              `variable-code` %in% varname     ~ "STUB",
#'                                              `variable-code` %in% figures_var ~ "FIGURES",
#'                                              TRUE ~ NA_character_
#'                                              )
#'                     ) %>%
#'       overwrite_sheet_data(temp_wb, "Variables")
#'
#'     codelists %>%
#'       dplyr::as_tibble() %>%
#'       dplyr::filter(`variable-code` %in% include_vars) %>%
#'       overwrite_sheet_data(temp_wb, "Codelists")
#'
#'     temp_workbook_path <- temp_xlsx_file()
#'     openxlsx::saveWorkbook(temp_wb, file = temp_workbook_path)
#'
#'
#'     #for all other varialbes than varname, the list should be filter down to the value in elimination
#'     # if the value is YES, no values should be filtered away.
#'     # If the value is NO, the variable canot be elimination, and so it should be
#'     # included in the dataset
#'
#'     # for elimination vars, take elimination values if they exist,
#'     # otherwise take all values
#'
#'     elimination_codes <-
#'       variables_long %>%
#'       dplyr::filter(`variable-code` %in% eliminate_vars) %>%
#'       dplyr::filter(! is.na(elimination)) %>%
#'       dplyr::select(`variable-code`, language, value = elimination) %>%
#'       dplyr::semi_join(x = codelists_long, y = . ,
#'                        by = c("variable-code", "language", "value")
#'                        ) %>%
#'       dplyr::distinct(`variable-code`, code)
#'
#'     include_everything_vars <-
#'       variables_long %>%
#'       dplyr::filter(`variable-code` %in% eliminate_vars) %>%
#'       dplyr::filter(is.na(elimination)) %>%
#'       dplyr::distinct(`variable-code`) %>%
#'       dplyr::pull(`variable-code`)
#'
#'     include_codes <-
#'       codelists_long %>%
#'       dplyr::filter(`variable-code` %in% include_everything_vars) %>%
#'       dplyr::distinct(`variable-code`, code)
#'
#'     df <-
#'       readRDS(temp_rds) %>%
#'       { if(length(eliminate_vars) > 0)
#'         dplyr::mutate(.data = ., id_ = dplyr::row_number()) %>%
#'         tidyr::pivot_longer(cols = all_of(eliminate_vars),
#'                             names_to = "variable-code", values_to = "code"
#'                             ) %>%
#'           dplyr::semi_join(dplyr::bind_rows(elimination_codes, include_codes),
#'                                  by = c("variable-code", "code")
#'                            ) %>%
#'           tidyr::pivot_wider(names_from = `variable-code`, values_from = code) %>%
#'           dplyr::select(-id_)
#'         else .
#'       } %>%
#'       dplyr::select(all_of(include_vars)) %>%
#'       dplyr::group_by(across(-all_of(figures_var))) %>%
#'       dplyr::summarise({{figures_var}} := sum(!!rlang::sym(figures_var), na.rm = TRUE),
#'                        .groups = "keep"
#'                        )
#'
#'     # if elimination var is YES, or has value in data, that value should be
#'     # selected otherwise all values should be selected. If no, the value
#'     # cannot be eliminated.
#'     pxmake(input = temp_workbook_path,
#'            out_path = file.path(out_dir, paste0('micro_', varname, '.px')),
#'            data = df
#'            )
#'   }
#' }
