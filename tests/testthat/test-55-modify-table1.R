test_that('Table1 keywords are modified and removed', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px() %>%
    charset("ANSI") %>%
    creation_date("2019-01-01 10:00") %>%
    matrix("BEXSTA") %>%
    decimals("1") %>%
    last_updated("2020-01-01 10:00") %>%
    next_update(format("2022-01-01 10:00", format='%Y%m%d %H:%M')) %>%
    subject_code('BEXSTA') %>%
    axis_version("2010") %>%
    codepage("iso-8859-1") %>%
    confidential("1") %>%
    copyright("YES") %>%
    showdecimals("3") %>%
    tableid("BEXSTAid") %>%
    update_frequency("yearly") %>%
    aggregallowed("YES") %>%
    autopen("NO") %>%
    descriptiondefault("NO")

  expect_identical(charset(x), "ANSI")
  expect_identical(creation_date(x), "2019-01-01 10:00")
  expect_identical(matrix(x), "BEXSTA")
  expect_identical(decimals(x), "1")
  expect_identical(last_updated(x), "2020-01-01 10:00")
  expect_identical(next_update(x), "2022-01-01 10:00")
  expect_identical(subject_code(x), "BEXSTA")
  expect_identical(axis_version(x), "2010")
  expect_identical(codepage(x), "iso-8859-1")
  expect_identical(confidential(x), "1")
  expect_identical(copyright(x), "YES")
  expect_identical(showdecimals(x), "3")
  expect_identical(tableid(x), "BEXSTAid")
  expect_identical(update_frequency(x), "yearly")
  expect_identical(aggregallowed(x), "YES")
  expect_identical(autopen(x), "NO")
  expect_identical(descriptiondefault(x), "NO")

  x2 <-
    x %>%
    charset(NULL) %>%
    creation_date(NULL) %>%
    last_updated(NULL) %>%
    next_update(NULL) %>%
    axis_version(NULL) %>%
    codepage(NULL) %>%
    confidential(NULL) %>%
    copyright(NULL) %>%
    showdecimals(NULL) %>%
    tableid(NULL) %>%
    update_frequency(NULL) %>%
    aggregallowed(NULL) %>%
    autopen(NULL) %>%
    descriptiondefault(NULL)

  expect_identical(charset(x2), NULL)
  expect_identical(creation_date(x2), NULL)
  expect_identical(last_updated(x2), NULL)
  expect_identical(next_update(x2), NULL)
  expect_identical(axis_version(x2), NULL)
  expect_identical(codepage(x2), NULL)
  expect_identical(confidential(x2), NULL)
  expect_identical(copyright(x2), NULL)
  expect_identical(showdecimals(x2), NULL)
  expect_identical(tableid(x2), NULL)
  expect_identical(update_frequency(x2), NULL)
  expect_identical(aggregallowed(x2), NULL)
  expect_identical(autopen(x2), NULL)
  expect_identical(descriptiondefault(x2), NULL)

  expect_error(matrix(x, NULL), regexp = "mandatory")
  expect_error(decimals(x, NULL), regexp = "mandatory")
  expect_error(subject_code(x, NULL), regexp = "mandatory")
})
