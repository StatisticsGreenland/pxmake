# DESCRIPTION

Inspect or change DESCRIPTION.

## Usage

``` r
px_description(x, value, validate)

# S3 method for class 'px'
px_description(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string to set the value for all languages or a
  data frame with columns 'language' and 'value' to set it for specific
  languages. If 'value' is missing, the current DESCRIPTION is returned.
  If NULL, DESCRIPTION is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A34%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C229%2C0%5D)

[`px_title()`](https://statisticsgreenland.github.io/pxmake/reference/px_title.px.md)

## Examples

``` r
# Set DESCRIPTION for all languages
x1 <-
  px(population_gl) |>
  px_description('Population')

# Print DESCRIPTION
px_description(x1)
#> [1] "Population"

# Set DESCRIPTION for individual languages
library(tibble)
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_description(tribble(~language, ~value,
                      'en', 'Population',
                      'kl', 'Innuttaasut'))
px_description(x2)
#> # A tibble: 2 × 2
#>   language value      
#>   <chr>    <chr>      
#> 1 en       Population 
#> 2 kl       Innuttaasut

# Remove DESCRIPTION
x3 <- px_description(x2, NULL)
px_description(x3)
#> NULL
```
