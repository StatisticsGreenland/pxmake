# Add or modify value

Modify a value in a row of a data frame based on its value in another
column. If the value is not found, a new row is added.

## Usage

``` r
modify_or_add_row(
  df,
  lookup_column,
  lookup_column_values,
  modify_column,
  new_value
)
```

## Arguments

- df:

  Data frame

- lookup_column:

  Column to look up

- lookup_column_values:

  Values to look up

- modify_column:

  Column to modify

- new_value:

  New value to modify/add to modify_column

## Value

A data frame
