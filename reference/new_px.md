# Create new px object

px constructor for internal functions

## Usage

``` r
new_px(
  languages,
  table1,
  table2,
  variables1,
  variables2,
  cells1,
  cells2,
  acrosscells,
  data
)
```

## Arguments

- languages:

  A data frame with language metadata.

- table1:

  A data frame with language independent table metadata.

- table2:

  A data frame with language dependent table metadata.

- variables1:

  A data frame with language independent variable metadata.

- variables2:

  A data frame with language dependent variable metadata.

- cells1:

  A data frame with language independent cells metadata.

- cells2:

  A data frame with language dependent cells metadata.

- acrosscells:

  A data frame with metadata that spans multiple cells.

- data:

  A data frame with data.

## Value

A px object
