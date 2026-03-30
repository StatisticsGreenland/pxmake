# DATASYMBOL2

Inspect or change DATASYMBOL2.

## Usage

``` r
px_datasymbol2(x, value, validate)

# S3 method for class 'px'
px_datasymbol2(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string to set the value for all languages or a
  data frame with columns 'language' and 'value' to set it for specific
  languages. If 'value' is missing, the current DATASYMBOL2 is returned.
  If NULL, DATASYMBOL2 is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A44%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C422%2C0%5D)

## Examples

``` r
# Set DATASYMBOL2 for all languages
x1 <-
  px(population_gl) |>
  px_datasymbol2('missing')

# Print DATASYMBOL2
px_datasymbol2(x1)
#> [1] "missing"

# Set DATASYMBOL2 for individual languages
library(tibble)
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_datasymbol2(tribble(~language, ~value,
                      'en', 'missing',
                      'kl', 'amigaataapput'))
px_datasymbol2(x2)
#> # A tibble: 2 × 2
#>   language value        
#>   <chr>    <chr>        
#> 1 en       missing      
#> 2 kl       amigaataapput

# Remove DATASYMBOL2
x3 <- px_datasymbol2(x2, NULL)
px_datasymbol2(x3)
#> NULL
```
