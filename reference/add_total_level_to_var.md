# Add total level to variable

Add a new level to a variables which is the sum of all other levels.

## Usage

``` r
add_total_level_to_var(
  df,
  variable,
  level_name = "Total",
  sum_var = "value",
  na.rm = TRUE
)
```

## Arguments

- df:

  Data frame to add total levels to.

- variable:

  Name of variable to add total level to.

- level_name:

  Total level name.

- sum_var:

  String, name of variable to sum over.

- na.rm:

  Optional. Logical. If TRUE, NAs are removed before summing.

## Value

A data frame
