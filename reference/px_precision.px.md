# PRECISION

Inspect or change PRECISION.

## Usage

``` r
px_precision(x, value, validate)

# S3 method for class 'px'
px_precision(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A data frame with the columns 'precision' and one or more of
  the columns: 'variable-code', and 'code'. If 'value' is missing, the
  current PRECISION is returned. If NULL, PRECISION is removed.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or data frame.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A51%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C385%2C0%5D)

## Examples

``` r
# Set PRECISION for a variable
library(tibble)
x1 <-
  population_gl |>
  px() |>
  px_precision(tribble(~`variable-code`, ~precision,
                       'gender', 2))

# Print PRECISION
px_precision(x1)
#> # A tibble: 2 × 3
#>   `variable-code` code   precision
#>   <chr>           <chr>      <dbl>
#> 1 gender          female         2
#> 2 gender          male           2

# Set PRECISION for a value
x2 <-
  x1 |>
  px_precision(tribble(~`variable-code`, ~code, ~precision,
                       'age', '2004', 3))
px_precision(x2)
#> # A tibble: 3 × 3
#>   `variable-code` code   precision
#>   <chr>           <chr>      <dbl>
#> 1 gender          female         2
#> 2 gender          male           2
#> 3 age             2004           3

# Remove PRECISION
x3 <- px_precision(x2, NULL)
px_precision(x3)
#> NULL
```
