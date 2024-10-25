test_that('Table2 keyword is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(px_last_updated(x), NULL)

  datetime <- '2020-01-01 10:00'

  x2 <- px_last_updated(x, datetime)
  expect_identical(px_last_updated(x2), datetime)

  x3 <- px_languages(x, c('en', 'dk'))
  expect_identical(px_last_updated(x3), NULL)

  x4 <- px_last_updated(x3, datetime)
  expect_identical(px_last_updated(x4),
                   dplyr::tibble(language = c("en", "dk"),
                                 value = '2020-01-01 10:00'
                                 )
                   )

  datetime_df <- dplyr::tibble(language = c('en', 'dk'),
                               value = c('2020-01-01 10:00',
                                         '2022-01-01 10:00'))

  x5 <- px_last_updated(x3, datetime_df)
  expect_identical(px_last_updated(x5), datetime_df)

  x6 <- px_last_updated(x5, NULL)
  expect_identical(px_last_updated(x6), NULL)

  expect_error(px_last_updated(x3, data.frame(language = c('sv', 'kl'),
                                           value = datetime)),
               regex = 'LANGUAGE'
               )
})

test_that('Other keywords are modified and removed', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  x2 <-
    x %>%
    px_contents('content') %>%
    px_description('description') %>%
    px_subject_area('subject area') %>%
    px_title('title') %>%
    px_units('units') %>%
    px_contact('Johan Ejstrud') %>%
    px_link('The Legend of Zelda') %>%
    px_infofile("infofile") %>%
    px_baseperiod("baseperiod") %>%
    px_stockfa("S") %>%
    px_cfprices("C") %>%
    px_source("Statistic Greenland")

  expect_identical(px_contents(x2), 'content')
  expect_identical(px_description(x2), 'description')
  expect_identical(px_subject_area(x2), 'subject area')
  expect_identical(px_title(x2), 'title')
  expect_identical(px_units(x2), 'units')
  expect_identical(px_contact(x2), 'Johan Ejstrud')
  expect_identical(px_link(x2), 'The Legend of Zelda')
  expect_identical(px_infofile(x2), "infofile")
  expect_identical(px_baseperiod(x2), "baseperiod")
  expect_identical(px_stockfa(x2), "S")
  expect_identical(px_cfprices(x2), "C")
  expect_identical(px_source(x2), "Statistic Greenland")

  x3 <-
    x2 %>%
    px_description(NULL) %>%
    px_contact(NULL) %>%
    px_link(NULL) %>%
    px_infofile(NULL) %>%
    px_baseperiod(NULL) %>%
    px_stockfa(NULL) %>%
    px_cfprices(NULL) %>%
    px_source(NULL)

  expect_identical(px_description(x3), NULL)
  expect_identical(px_contact(x3), NULL)
  expect_identical(px_link(x3), NULL)
  expect_identical(px_infofile(x3), NULL)
  expect_identical(px_baseperiod(x3), NULL)
  expect_identical(px_stockfa(x3), NULL)
  expect_identical(px_cfprices(x3), NULL)
  expect_identical(px_source(x3), NULL)

  expect_error(px_contents(x3, NULL), regex = 'mandatory')
  expect_error(px_units(x3, NULL),    regex = 'mandatory')
  expect_error(px_subject_area(x3, NULL), regex = 'mandatory')
})

test_that('Either TITLE or DESCRIPTION should be defined', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(px_title(x), "")

  expect_error(px_title(x, NULL), regex = 'cannot be removed unless')

  x2 <-
    px_description(x, "description") %>%
    px_title(NULL)

  expect_identical(px_description(x2), "description")

  expect_error(px_description(x2, NULL), regex = 'cannot be removed unless')
})

