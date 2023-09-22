#' Create microdata
#'
#' Turn a dataset and its metadata into a series of px-files, one for each
#' variable in the original dataset, except time vars.
#'
#' @inheritParams metamake
#' @param out_dir Directory to save px-files in.
#'
#' @returns Nothing
micromake <- function(input, out_dir) {
  temp_excel <- temp_xlsx_file()
  temp_rds   <- temp_rds_file()

  metamake(input, out_path = temp_excel, data_path = temp_rds)

  wb <- openxlsx::loadWorkbook(temp_excel)

  variables <- openxlsx::readWorkbook(wb, sheet="Variables") %>% dplyr::as_tibble()

  variables_long <-
    variables %>%
    dplyr::mutate(across(-one_of("order"), as.character)) %>%
    tidyr::pivot_longer(cols = -c(pivot, order, `variable-code`, type),
                        names_to = c("language", "keyword"),
                        names_pattern = "^([[:alpha:]]+)_(.*)$"
                        ) %>%
    tidyr::pivot_wider(names_from = "keyword")

  codelists <- openxlsx::readWorkbook(wb, sheet="Codelists")

  codelists_long <-
    codelists %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = ends_with("_code-label"),
                        names_to = c("language"),
                        names_pattern = "^([[:alpha:]]+)_.*$"
                        )

  figures_var <-
    variables %>%
    dplyr::filter(toupper(pivot) == "FIGURES") %>%
    dplyr::pull(`variable-code`)

  time_var <-
    variables %>%
    dplyr::filter(toupper(type) == "TIME") %>%
    dplyr::pull(`variable-code`)

  stub_heading_vars <-
    variables %>%
    dplyr::filter(pivot %in% c("STUB", "HEADING")) %>%
    dplyr::pull(`variable-code`)

  micro_vars <- setdiff(stub_heading_vars, time_var)

  for (varname in micro_vars) {
    include_vars <- c(varname, time_var, figures_var)

    temp_wb <- wb

    overwrite_sheet_data <- function(df, workbook, sheet_name) {
      openxlsx::removeWorksheet(workbook, sheet_name)
      openxlsx::addWorksheet(workbook, sheet_name)
      openxlsx::writeDataTable(workbook, sheet_name, df)
    }

    variables %>%
      dplyr::filter(`variable-code` %in% include_vars) %>%
      dplyr::mutate(pivot = dplyr::case_when(`variable-code` %in% time_var    ~ "HEADING",
                                             `variable-code` %in% varname     ~ "STUB",
                                             `variable-code` %in% figures_var ~ "FIGURES",
                                             TRUE ~ NA_character_
                                             )
                    ) %>%
      overwrite_sheet_data(temp_wb, "Variables")

    codelists %>%
      dplyr::as_tibble() %>%
      dplyr::filter(`variable-code` %in% include_vars) %>%
      overwrite_sheet_data(temp_wb, "Codelists")

    temp_workbook_path <- temp_xlsx_file()
    openxlsx::saveWorkbook(temp_wb, file = temp_workbook_path)

    df <-
      readRDS(temp_rds) %>%
      dplyr::filter(dplyr::if_all(setdiff(micro_vars, varname), ~ . != 'T')) %>% #elimination
      dplyr::select(all_of(include_vars)) %>%
      dplyr::group_by(across(-all_of(figures_var))) %>%
      dplyr::summarise({{figures_var}} := sum(!!rlang::sym(figures_var), na.rm = TRUE),
                       .groups = "keep"
                       )

    # if elimination var is YES, or has value in data, that value should be
    # selected otherwise all values should be selected
    pxmake(input = temp_workbook_path,
           out_path = file.path(out_dir, paste0('micro_', varname, '.px')),
           data = df
           )
  }
}


