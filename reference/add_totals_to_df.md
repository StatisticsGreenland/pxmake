# Add total levels to multiple variables

Wrapper around
[add_total_level_to_var](https://statisticsgreenland.github.io/pxmake/reference/add_total_level_to_var.md)
to add levels to total level to multiple variables.

## Usage

``` r
add_totals_to_df(df, variables, level_names, sum_var = "value", na.rm = TRUE)
```

## Arguments

- df:

  Data frame to add total levels to.

- variables:

  List of variables to add total levels to.

- level_names:

  Names of total levels. Should have length 1 or same length as
  `variables`.

- sum_var:

  String, name of variable to sum over.

- na.rm:

  Optional. Logical. If TRUE, NAs are removed before summing.
