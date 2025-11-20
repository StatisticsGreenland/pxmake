# VARIABLE-TYPE

Inspect or change VARIABLE-TYPE.

## Usage

``` r
px_variable_type(x, value, validate)

# S3 method for class 'px'
px_variable_type(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  A data frame with columns 'variable-code' and 'variable-type'. If
  value is missing, the current VARIABLE-TYPE is returned. If NULL, all
  VARIABLE-TYPE is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A56%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C499%2C0%5D)

## Examples

``` r
library(tibble)

# Set VARIABLE-TYPE
x1 <-
  px(population_gl) |>
  px_variable_type(tibble('variable-code' = 'year', 'variable-type' = 'time'))

# Print VARIABLE-TYPE
px_variable_type(x1)
#> # A tibble: 1 × 2
#>   `variable-code` `variable-type`
#>   <chr>           <chr>          
#> 1 year            time           

# Remove VARIABLE-TYPE
x2 <- px_variable_type(x1, NULL)
px_variable_type(x2)
#> NULL
```
