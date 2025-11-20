# ELIMINATION

Inspect or change ELIMINATION.

## Usage

``` r
px_elimination(x, value, validate)

# S3 method for class 'px'
px_elimination(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. Use character to set ELIMINATION for all STUB and HEADING
  variables. Use a data frame with columns 'variable-code' and
  'elimination' to set ELIMINATION for individual variables. If value is
  missing, the current ELIMINATION is returned. If NULL, ELIMINATION is
  removed for all variables.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A46%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C525%2C0%5D)

## Examples

``` r
library(tibble)

# Set ELIMINATION
x1 <-
  px(population_gl) |>
  px_elimination(tribble(~`variable-code`, ~elimination,
                         "gender", "T",
                         "age", "YES"
                        )
                )

# Print ELIMINATION
px_elimination(x1)
#> # A tibble: 2 × 2
#>   `variable-code` elimination
#>   <chr>           <chr>      
#> 1 gender          T          
#> 2 age             YES        

# Remove ELIMINATION
x2 <- px_elimination(x1, NULL)
px_elimination(x2)
#> NULL
```
