test_that('Table1 keywords are modified and removed', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px() %>%
    px_charset("ANSI") %>%
    px_creation_date("2019-01-01 10:00") %>%
    px_matrix("BEXSTA") %>%
    px_decimals("1") %>%
    px_last_updated("2020-01-01 10:00") %>%
    px_next_update(format("2022-01-01 10:00", format='%Y%m%d %H:%M')) %>%
    px_subject_code('BEXSTA') %>%
    px_axis_version("2010") %>%
    px_codepage("iso-8859-1") %>%
    px_confidential("1") %>%
    px_copyright("YES") %>%
    px_showdecimals("3") %>%
    px_tableid("BEXSTAid") %>%
    px_update_frequency("yearly") %>%
    px_aggregallowed("YES") %>%
    px_autopen("NO") %>%
    px_descriptiondefault("NO") %>%
    px_official_statistics("YES")

  expect_identical(px_charset(x), "ANSI")
  expect_identical(px_creation_date(x), "2019-01-01 10:00")
  expect_identical(px_matrix(x), "BEXSTA")
  expect_identical(px_decimals(x), "1")
  expect_identical(px_last_updated(x), "2020-01-01 10:00")
  expect_identical(px_next_update(x), "2022-01-01 10:00")
  expect_identical(px_subject_code(x), "BEXSTA")
  expect_identical(px_axis_version(x), "2010")
  expect_identical(px_codepage(x), "iso-8859-1")
  expect_identical(px_confidential(x), "1")
  expect_identical(px_copyright(x), "YES")
  expect_identical(px_showdecimals(x), "3")
  expect_identical(px_tableid(x), "BEXSTAid")
  expect_identical(px_update_frequency(x), "yearly")
  expect_identical(px_aggregallowed(x), "YES")
  expect_identical(px_autopen(x), "NO")
  expect_identical(px_descriptiondefault(x), "NO")
  expect_identical(px_official_statistics(x), "YES")

  x2 <-
    x %>%
    px_charset(NULL) %>%
    px_creation_date(NULL) %>%
    px_last_updated(NULL) %>%
    px_next_update(NULL) %>%
    px_axis_version(NULL) %>%
    px_codepage(NULL) %>%
    px_confidential(NULL) %>%
    px_copyright(NULL) %>%
    px_showdecimals(NULL) %>%
    px_tableid(NULL) %>%
    px_update_frequency(NULL) %>%
    px_aggregallowed(NULL) %>%
    px_autopen(NULL) %>%
    px_descriptiondefault(NULL) %>%
    px_official_statistics(NULL)

  expect_identical(px_charset(x2), NULL)
  expect_identical(px_creation_date(x2), NULL)
  expect_identical(px_last_updated(x2), NULL)
  expect_identical(px_next_update(x2), NULL)
  expect_identical(px_axis_version(x2), NULL)
  expect_identical(px_codepage(x2), NULL)
  expect_identical(px_confidential(x2), NULL)
  expect_identical(px_copyright(x2), NULL)
  expect_identical(px_showdecimals(x2), NULL)
  expect_identical(px_tableid(x2), NULL)
  expect_identical(px_update_frequency(x2), NULL)
  expect_identical(px_aggregallowed(x2), NULL)
  expect_identical(px_autopen(x2), NULL)
  expect_identical(px_descriptiondefault(x2), NULL)
  expect_identical(px_official_statistics(x2), NULL)

  expect_error(px_matrix(x, NULL), regexp = "mandatory")
  expect_error(px_decimals(x, NULL), regexp = "mandatory")
  expect_error(px_subject_code(x, NULL), regexp = "mandatory")
})
