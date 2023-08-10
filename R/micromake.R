#' Create microdata
#'
#' Turn a dataset and its metadata into a series of px-files, one for each
#' variable in the original data set.
#'
#' @inheritParams pxmake
#' @param out_dir Directory to save px-files in.
#'
#' @returns Nothing
micromake <- function(input, data) {

  # convert to rds
  # how to store additional information required for microdata conversion?

  # Create rds file for each variable


  rds <- pxmake(get_metadata_path('bexsta'), data = get_data_path('bexsta'))

  get_one_var <- function(varname) {
    name_relation <-
      rds$metadata %>%
      dplyr::filter(keyword == "VARIABLECODE") %>%
      tidyr::unnest(value) %>%
      dplyr::select(`variable-code` = value, variable, language)

    vars <-
      dplyr::distinct(name_relation, `variable-code`) %>%
      dplyr::pull(`variable-code`)

    metadata <-
      rds$metadata %>%
      dplyr::left_join(name_relation, by = c('variable', 'language'))

    head_and_stub <-
      rds$metadata %>%
      dplyr::filter(keyword %in% c("STUB", "HEADING")) %>%
      tidyr::unnest(value) %>%
      dplyr::left_join(name_relation, by = c('value' = 'variable', 'language')) %>%
      dplyr::distinct(`variable-code`) %>%
      dplyr::pull(`variable-code`)

    figures_var <- setdiff(vars, head_and_stub)

    # also include timevar

    # loop
    metadata_new <-
      metadata %>%
      dplyr::filter(is.na(`variable-code`) | `variable-code` == varname)

    data_new <-
      rds$data %>%
      dplyr::select(all_of(c(varname, figures_var)))

    return(list('metadata' = metadata_new, 'data' = data_new))
  }



}
