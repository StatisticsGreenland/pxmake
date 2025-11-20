# STOCKFA

Inspect or change STOCKFA.

## Usage

``` r
px_stockfa(x, value, validate)

# S3 method for class 'px'
px_stockfa(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string to set the value for all languages or a
  data frame with columns 'language' and 'value' to set it for specific
  languages. If 'value' is missing, the current STOCKFA is returned. If
  NULL, STOCKFA is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A54%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C646%2C0%5D)

## Examples

``` r
# Set STOCKFA for all languages
x1 <-
  px(population_gl) |>
  px_stockfa('S')

# Print STOCKFA
px_stockfa(x1)
#> [1] "S"

# Set STOCKFA for individual languages
library(tibble)
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_stockfa(tribble(~language, ~value,
                      'en', 'S',
                      'kl', 'F'))
px_stockfa(x2)
#> # A tibble: 2 × 2
#>   language value
#>   <chr>    <chr>
#> 1 en       S    
#> 2 kl       F    

# Remove STOCKFA
x3 <- px_stockfa(x2, NULL)
px_stockfa(x3)
#> NULL
```
