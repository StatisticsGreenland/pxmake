get_px_metadata_regex <- function() {
  paste0("(?<keyword>[[:upper:]-]+)",                    # Leading keyword
         "(?:\\[)?(?<language>[[:alpha:]_-]+)?(?:\\])?", # Maybe language
         "(?:\\()?(?<variable>[^\\(\\)]+)?(?:\\))?",     # Maybe sub-key
         "=",                                            # definitely =
         "(?<value>[^;]*)",                              # Up to ending ;
         "(?:;$)?"                                       # Maybe ;
  )
}

#' Create an Excel metadata workbook from a px-file
#'
#' Turn a px-file into an Excel metadata workbook. If pxmake() is run on that
#' workbook it turns back into the same px-file.
#'
#' @param px_file_path Path to px file
#' @param out_path Path to save file at
#'
#' @returns Nothing
metamake <- function(px_file_path, out_path) {
  lines <- readLines(px_file_path)

  ## Split metadata in data cube and heading
  data_line_index <- stringr::str_which(lines, '^DATA=$')

  if(length(data_line_index) != 1) {
    stop("unhandled error")
  }

  metadata_lines <- lines[c(1:data_line_index)]
  data_lines     <- lines[c((data_line_index+1):length(lines))]

  tmp_metadata <-
    metadata_lines %>%
    str_match(get_px_metadata_regex()) %>%
    magrittr::extract(,-1) %>% # remove full match column
    as_tibble() %>%
    mutate(across(everything(), ~str_replace_all(., '"', '')),
           value = str_split(value, ",")
           )

  main_language <-
    tmp_metadata %>%
    filter(keyword == "LANGUAGE") %>%
    pull(value) %>%
    unlist()

  tmp_metadata2 <-
    tmp_metadata %>%
    mutate(language = replace_na(language, main_language))

  head_stub <-
    tmp_metadata2 %>%
    filter(keyword %in% c("HEADING", "STUB")) %>%
    unnest(value) %>%
    rename(long_name = value) %>%
    group_by(keyword, language) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    select(-variable)

  tmp <-
    head_stub %>%
    filter(language == main_language) %>%
    select(-language) %>%
    rename(variable = long_name) %>%
    right_join(head_stub, by = join_by(keyword, index), multiple = "all")

  position <-
    tmp %>%
    mutate(position = paste0(substr(keyword, 1, 1), index)) %>%
    distinct(position, variable)

  name_relation <- tmp %>% select(long_name, variable)

  metadata <-
    tmp_metadata2 %>%
    rename(long_name = variable) %>%
    left_join(name_relation, by = "long_name")

  ###
  ### Make variables sheet
  ###
  long_name <-
    metadata %>%
    distinct(variable, language, long_name) %>%
    drop_na() %>%
    pivot_wider(names_from = language,
                names_glue = "{language}_long_name",
                values_from = long_name
    )

  note_elimination_domain <-
    metadata %>%
    filter(keyword %in% c("NOTE", "ELIMINATION", "DOMAIN", "HEADING")) %>%
    drop_na(long_name) %>%
    select(-long_name) %>%
    unnest(value) %>%
    pivot_wider(names_from = c(language, keyword),
                names_glue = "{language}_{tolower(keyword)}",
                values_from = value
                )

  time_vars <-
    metadata %>%
    filter(keyword == "TIMEVAL", language == main_language) %>%
    pull(variable)

  variables <-
    position %>%
    left_join(tibble(variable = time_vars, type = "time"), by = join_by(variable)) %>%
    left_join(long_name, by = join_by(variable)) %>%
    left_join(note_elimination_domain, by = join_by(variable)) %>%
    bind_rows(tibble(variable = "_value", type = "figures"))


  ###
  ### Make Codelists
  ###
  codes <-
    metadata %>%
    filter(keyword %in% c("CODES"), language == main_language) %>%
    unnest(value) %>%
    rename(code = value) %>%
    group_by(variable) %>%
    mutate(sortorder = row_number()) %>%
    ungroup() %>%
    select(variable, code, sortorder)

  values <-
    metadata %>%
    filter(keyword %in% c("VALUES")) %>%
    unnest(value) %>%
    group_by(variable, language) %>%
    mutate(sortorder = row_number()) %>%
    select(variable, value, language, sortorder)

  codelsit <-
    codes %>%
    right_join(values, by = join_by(variable, sortorder), multiple = "all") %>%
    drop_na(code) %>%
    pivot_wider(names_from = language, names_glue = "{language}_code_label")

  ###
  ### Make General
  ###


}


# px_file_path <- get_pxfile_path('BEXSTA')
# library(tidyverse)
