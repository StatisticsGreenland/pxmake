smallest_larger_value <- function(vec, value) {
  min(vec[vec > value])
}

value_set_interval <- function(lines, head_line) {
  breaks <-
    c(stringr::str_which(lines, '^[:space:]*$'), length(lines) + 1)

  (head_line+1):(smallest_larger_value(breaks, head_line)-1)
}

get_chunk <- function(lines, head_line) {
  lines[value_set_interval(lines, head_line)]
}

extract_values <- function(lines) {
  lines %>%
    stringr::str_match("=(.*)$") %>%
    magrittr::extract(, 2)
}

extract_chunk <- function(lines, heading, key = NULL) {
  head_line <- stringr::str_which(lines, stringr::fixed(heading))

  chunk <- get_chunk(lines, head_line)

  if (! is.null(key)) {
     chunk <-  stringr::str_subset(chunk, stringr::str_glue("^{key}=.*$"))
  }

  extract_values(chunk)
}

new_classification <- function(name, prestext, domain, df) {
  vs <- list(name     = as.character(name),
             prestext = as.character(prestext),
             domain   = as.character(domain), #list
             # type     = "V", only support V
             # aggreg   = list(), # list of aggregation objects
             # values   = list() # named list list(value_1 = c(), value_2 = c(), ...)
             df = df#dataset with names
               )

  structure(vs, class = "classification")
}

df_from_agg <- function(path) {
  agg_lines <- readLines_guess_encoding(path)

  aggregation_name <-
    extract_chunk(agg_lines, '[Aggreg]', 'Name') %>%
    stringr::str_trim()

  values <-
    extract_chunk(agg_lines, '[Aggtext]')

  df <- dplyr::tibble(code                = as.character(),
                      !!aggregation_name := factor(levels = values,
                                                   ordered = TRUE
                                                   )
                      )

  for (value in values) {
    df <- dplyr::bind_rows(df,
                           dplyr::tibble(code = extract_chunk(agg_lines,
                                                              paste0("[", value, "]")
                                                              ),
                                         !!aggregation_name := factor(value,
                                                                      levels = values,
                                                                      ordered = TRUE
                                                                      )
                                         )
                           )
  }

  return(df)
}

classification_from_path <- function(vs_path, agg_paths = c()) {
  agg_df <-
    agg_paths %>%
    purrr::map(df_from_agg) %>%
    purrr::reduce(dplyr::full_join, by = "code")

  vs_lines  <- readLines_guess_encoding(vs_path)

  vs_df <-
    dplyr::tibble(code = extract_chunk(vs_lines, '[Valuecode]'),
                  text = extract_chunk(vs_lines, '[Valuetext]')
                  )

  new_classification(name     = extract_chunk(vs_lines, '[Descr]', 'Name'),
                     prestext = extract_chunk(vs_lines, '[Descr]', 'Prestext'),
                     domain   = extract_chunk(vs_lines, '[Domain]'),
                     df = dplyr::full_join(vs_df, agg_df, by = 'code')
                     )
}

enumerate_lines <- function(lines) {
  stringr::str_glue("{seq_along(lines)}={lines}") %>%
    paste(collapse = "\n")
}

write_value_set <- function(c, directory) {
  filename <- paste0(directory, c$name, ".vs")

  aggregation_names <-
    c$df %>%
    dplyr::select(-code, -text) %>%
    names() %>%
    paste0(".agg") %>%
    enumerate_lines()

  value_codes <-
    c$df %>%
    dplyr::pull(code) %>%
    enumerate_lines()

  value_text <-
    c$df %>%
    dplyr::pull(text) %>%
    enumerate_lines()

  vs_lines <-
    stringr::str_glue(
    "[Descr]",
    "Name={c$name}",
    "Prestext={c$prestext}",
    "Type=V",
    blank_line(),
    "[Aggreg]",
    aggregation_names,
    blank_line(),
    "[Domain]",
    enumerate_lines(c$domain),
    blank_line(),
    "[Valuecode]",
    value_codes,
    blank_line(),
    "[Valuetext]",
    value_text,
    .sep = "\n"
    )

  file_connection <- file(filename, encoding = "ISO-8859-1")
  writeLines(vs_lines, file_connection)
  close(file_connection)
}

blank_line <- function() " "

write_aggregation <- function(aggregation, c, directory) {
  filename <- paste0(directory, aggregation, "_px.agg")

  groups <- levels(c$df[[aggregation]])

  agg_texts <-
    c$df %>%
    dplyr::distinct(code, !!rlang::sym(aggregation)) %>%
    dplyr::arrange(as.character(!!rlang::sym(aggregation))) %>%
    tidyr::pivot_wider(names_from = all_of(aggregation),
                       values_from  = "code",
                       values_fn = list
                       ) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(str =
                    stringr::str_glue("[{name}]\n",
                                      "{paste(enumerate_lines(value), collapse = '\n')}"
                    )
    ) %>%
    dplyr::pull(str) %>%
    paste0(collapse = paste0("\n", blank_line(), "\n"))

  agg_lines <-
    stringr::str_glue(
      "[Aggreg]",
      "Name={aggregation}",
      "Valueset={c$name}",
      enumerate_lines(groups),
      blank_line(),
      "[Aggtext]",
      enumerate_lines(groups),
      blank_line(),
      agg_texts,
      .sep = "\n"
    )

  file_connection <- file(filename, encoding = "ISO-8859-1")
  writeLines(agg_lines, file_connection)
  close(file_connection)
}

write_aggregations <- function(c, directory) {
  aggregations <-
    c$df %>%
    dplyr::select(-code, -text) %>%
    names()

  purrr::walk(aggregations, write_aggregation, c = c, directory = directory)
}

save_classification <- function(c, directory) {
  write_value_set(c, directory)
  write_aggregations(c, directory)
}

# vs_path <- "C:\\Users\\Johan Ejstrud\\Dropbox\\repo\\pxmake\\fsted\\fsted\\PXVSda_fsted.vs"
# agg_paths <- list.files("C:\\Users\\Johan Ejstrud\\Dropbox\\repo\\pxmake\\fsted\\fsted\\", pattern = "fødested.*.agg", full.names = TRUE)
#
# c <- classification_from_path(vs_path, agg_paths)
#
# save_classification(c, directory = "C:\\Users\\Johan Ejstrud\\Dropbox\\repo\\pxmake\\fsted\\")
