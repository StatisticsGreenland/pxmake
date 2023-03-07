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

  metadata <-
    metadata_lines %>%
    tidyr::as_tibble() %>%
    tidyr::separate_wider_delim(value, "=", names = c("key", "value")) %>%
    tidyr::separate_wider_regex(key, c(keyword = "[:upper:]*",
                                       "\\[", language = "[:alpha:]+", "\\]",
                                       '\\("', variable = ".+",
                                       '"\\)'
                                       ),
                                too_few = "align_start") %>%
    dplyr::mutate(value = str_replace(value, ";$", ""))

  metadata_lines %>%
    str_match(get_px_metadata_regex()) %>%
    as_tibble() %>%
    select(-V1)


  main_language <-
    metadata %>%
    filter(keyword == "LANGUAGE") %>%
    pull(value)

  position_variable <-
    metadata %>%
    filter(keyword %in% c("HEADING", "STUB")) %>%
 #   filter(langauge == )
    mutate(value = str_split(value, ",")) %>%
    unnest(value) %>%
    group_by(keyword) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    mutate(position = paste0(substr(keyword, 1, 1),
                             index
                             ),
           variable = value,
           type     = ""
           ) %>%
    select(position, variable)




}
