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


  get_one_var('gender')
  get_one_var('place of birth')
}

get_one_var <- function(varname) {
  rds <- pxmake(get_metadata_path('bexsta'), data = get_data_path('bexsta'))

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

  time_var <-
    metadata %>%
    dplyr::filter(keyword == "TIMEVAL") %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  head_and_stub <-
    rds$metadata %>%
    dplyr::filter(keyword %in% c("STUB", "HEADING")) %>%
    tidyr::unnest(value) %>%
    dplyr::left_join(name_relation, by = c('value' = 'variable', 'language')) %>%
    dplyr::distinct(`variable-code`) %>%
    dplyr::pull(`variable-code`)

  figures_var <- setdiff(vars, head_and_stub)

  # also include timevar

  include_vars <- c(varname, figures_var, time_var)

  # loop
  new_head_stub <-
    metadata %>%
    dplyr::filter(keyword %in% c("STUB", "HEADING")) %>%
    dplyr::select(-`variable-code`) %>%
    tidyr::unnest(value) %>%
    dplyr::left_join(name_relation, by = c('value' = 'variable', 'language')) %>%
    dplyr::filter(`variable-code` %in% include_vars) %>%
    dplyr::select(-`variable-code`) %>%
    dplyr::group_by(across(-value)) %>%
    dplyr::summarise(value = list(value), .groups = "keep")


  #use variable and stub, and timevar as heading
  stub_df <-
    name_relation %>%
    dplyr::filter(`variable-code` %in% varname) %>%
    dplyr::mutate(keyword = "STUB") %>%
    dplyr::rename(value = variable) %>%
    wrap_varaible_in_list(value)

  heading_df <-
    name_relation %>%
    dplyr::filter(`variable-code` %in% time_var) %>%
    dplyr::mutate(keyword = "HEADING") %>%
    dplyr::rename(value = variable) %>%
    wrap_varaible_in_list(value)

  metadata_new <-
    metadata %>%
    dplyr::filter(is.na(`variable-code`) | `variable-code` %in% include_vars) %>%
    dplyr::filter(!keyword %in% c("STUB", "HEADING")) %>%
    dplyr::bind_rows(stub_df,
                     heading_df
                     )

  data_new <-
    rds$data %>%
    dplyr::as_tibble() %>%
    dplyr::filter(dplyr::if_all(everything(), ~ . != 'T')) %>% #elimination
    dplyr::select(all_of(include_vars)) %>%
    dplyr::group_by(across(-all_of(figures_var))) %>%
    dplyr::summarise({{figures_var}} := sum(!!rlang::sym(figures_var), na.rm = TRUE))

  rds_new <- list('metadata' = metadata_new, 'data' = data_new)

  pxmake(rds_new, paste0(varname, '.px'))
}
