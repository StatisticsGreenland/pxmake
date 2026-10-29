# Change language in table

Change language in table

## Usage

``` r
modify_languages_in_table(
  df,
  new_languages,
  keep_vars,
  align_df,
  main_language
)
```

## Arguments

- df:

  A data frame to change languages in

- new_languages:

  A character vector

- keep_vars:

  A character vector

- align_df:

  A data frame to align with

- main_language:

  A character string. Language to copy values from when adding new
  languages. If not in `df`, the first language in `df` is used.

## Value

A data frame
