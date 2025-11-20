# LINK

Inspect or change LINK.

## Usage

``` r
px_link(x, value, validate)

# S3 method for class 'px'
px_link(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string to set the value for all languages or a
  data frame with columns 'language' and 'value' to set it for specific
  languages. If 'value' is missing, the current LINK is returned. If
  NULL, LINK is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A49%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C453%2C0%5D)

## Examples

``` r
# Set LINK for all languages
x1 <-
  px(population_gl) |>
  px_link('https://stat.gl/?lang=en')

# Print LINK
px_link(x1)
#> [1] "https://stat.gl/?lang=en"

# Set LINK for individual languages
library(tibble)
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_link(tribble(~language, ~value,
                      'en', 'https://stat.gl/?lang=en',
                      'kl', 'https://stat.gl/'))
px_link(x2)
#> # A tibble: 2 × 2
#>   language value                   
#>   <chr>    <chr>                   
#> 1 en       https://stat.gl/?lang=en
#> 2 kl       https://stat.gl/        

# Remove LINK
x3 <- px_link(x2, NULL)
px_link(x3)
#> NULL
```
