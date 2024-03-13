test_that('Table2 keyword is modified', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(last_updated(x), NULL)

  datetime <- '2020-01-01 10:00'

  x2 <- last_updated(x, datetime)
  expect_identical(last_updated(x2), datetime)

  x3 <- languages(x, c('en', 'dk'))
  expect_identical(last_updated(x3), NULL)

  x4 <- last_updated(x3, datetime)
  expect_identical(last_updated(x4),
                   dplyr::tibble(language = c("en", "dk"),
                                 value = '2020-01-01 10:00'
                                 )
                   )

  datetime_df <- dplyr::tibble(language = c('en', 'dk'),
                               value = c('2020-01-01 10:00',
                                         '2022-01-01 10:00'))

  x5 <- last_updated(x3, datetime_df)
  expect_identical(last_updated(x5), datetime_df)

  x6 <- last_updated(x5, NULL)
  expect_identical(last_updated(x6), NULL)

  expect_error(last_updated(x3, data.frame(language = c('sv', 'kl'),
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
    contents('content') %>%
    description('description') %>%
    subject_area('subject area') %>%
    title('title') %>%
    units('units') %>%
    contact('Johan Ejstrud') %>%
    link('The Legend of Zelda') %>%
    infofile("infofile") %>%
    map('X marks the spot')

  expect_identical(contents(x2), 'content')
  expect_identical(description(x2), 'description')
  expect_identical(subject_area(x2), 'subject area')
  expect_identical(title(x2), 'title')
  expect_identical(units(x2), 'units')
  expect_identical(contact(x2), 'Johan Ejstrud')
  expect_identical(link(x2), 'The Legend of Zelda')
  expect_identical(infofile(x2), "infofile")
  expect_identical(map(x2), 'X marks the spot')


  x3 <-
    x2 %>%
    description(NULL) %>%
    contact(NULL) %>%
    link(NULL) %>%
    infofile(NULL) %>%
    map(NULL)

  expect_identical(description(x3), NULL)
  expect_identical(contact(x3), NULL)
  expect_identical(link(x3), NULL)
  expect_identical(infofile(x3), NULL)
  expect_identical(map(x3), NULL)

  expect_error(contents(x3, NULL), regex = 'mandatory')
  expect_error(units(x3, NULL),    regex = 'mandatory')
  expect_error(subject_area(x3, NULL), regex = 'mandatory')
})

test_that('Either TITLE or DESCRIPTION should be defined', {
  x <-
    'BEXSTA' %>%
    get_data_path() %>%
    readRDS() %>%
    px()

  expect_identical(title(x), "")

  expect_error(title(x, NULL), regex = 'cannot be removed unless')

  x2 <-
    description(x, "description") %>%
    title(NULL)

  expect_identical(description(x2), "description")

  expect_error(description(x2, NULL), regex = 'cannot be removed unless')
})

