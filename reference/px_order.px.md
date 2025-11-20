# Change value order

Inspect or change ORDER.

## Usage

``` r
px_order(x, value, validate)

# S3 method for class 'px'
px_order(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A data frame with the columns 'order' and one or more of the
  columns: 'variable-code', and 'code'. If 'value' is missing, the
  current ORDER is returned. If NULL, ORDER is removed.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or data frame.

## Examples

``` r
# Set ORDER for a variable
library(tibble)
x1 <-
  population_gl |>
  px() |>
  px_order(tribble(~`variable-code`, ~order,
                       'gender', 8))

# Print ORDER
px_order(x1)
#> # A tibble: 10 × 3
#>    `variable-code` code   order
#>    <chr>           <chr>  <dbl>
#>  1 gender          female     8
#>  2 gender          male       8
#>  3 age             0-6        1
#>  4 age             17-24      2
#>  5 age             25-64      3
#>  6 age             65+        4
#>  7 age             7-16       5
#>  8 year            2004       1
#>  9 year            2014       2
#> 10 year            2024       3

# Set ORDER for a value
x2 <-
  x1 |>
  px_order(tribble(~`variable-code`, ~code, ~order,
                       'age', '2004', 9))
px_order(x2)
#> # A tibble: 11 × 3
#>    `variable-code` code   order
#>    <chr>           <chr>  <dbl>
#>  1 gender          female     8
#>  2 gender          male       8
#>  3 age             0-6        1
#>  4 age             17-24      2
#>  5 age             25-64      3
#>  6 age             65+        4
#>  7 age             7-16       5
#>  8 age             2004       9
#>  9 year            2004       1
#> 10 year            2014       2
#> 11 year            2024       3

# Remove ORDER
x3 <- px_order(x2, NULL)
px_order(x3)
#> NULL
```
