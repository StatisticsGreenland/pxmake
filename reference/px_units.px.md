# UNITS

Inspect or change UNITS.

## Usage

``` r
px_units(x, value, validate)

# S3 method for class 'px'
px_units(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string to set the value for all languages or a
  data frame with columns 'language' and 'value' to set it for specific
  languages. If 'value' is missing, the current UNITS is returned. If
  NULL, an error is thrown because UNITS cannot be removed.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object, a character string, or a data frame.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A36%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C359%2C0%5D)

## Examples

``` r
# Set UNITS for all languages
x1 <-
  px(population_gl) |>
  px_units('persons')

# Print UNITS
px_units(x1)
#> [1] "persons"

# Set UNITS for individual languages
library(tibble)
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_units(tribble(~language, ~value,
                      'en', 'persons',
                      'kl', 'inuit amerlassusaat'))
px_units(x2)
#> # A tibble: 2 × 2
#>   language value              
#>   <chr>    <chr>              
#> 1 en       persons            
#> 2 kl       inuit amerlassusaat
```
