# DOMAIN

Inspect or change DOMAIN.

## Usage

``` r
px_domain(x, value, validate)

# S3 method for class 'px'
px_domain(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string or data frame.

  - Use character to set DOMAIN for all languages and STUB/HEADING
    variables.

  - Use data frame with columns 'variable-code', 'language' and 'domain'
    to set DOMAIN for specific variables.

  - If missing, the current DOMAIN is returned.

  - If NULL, DOMAIN is removed for all variables.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A45%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C214%2C0%5D)

## Examples

``` r
# Set DOMAIN for all languages
x1 <-
  px(population_gl) |>
  px_domain('aggregation1')

# Print DOMAIN
px_domain(x1)
#> # A tibble: 3 × 2
#>   `variable-code` domain      
#>   <chr>           <chr>       
#> 1 gender          aggregation1
#> 2 age             aggregation1
#> 3 year            aggregation1

# Set DOMAIN for individual variables
library(tibble)
x2 <-
  x1 |>
  px_domain(tribble(~`variable-code`, ~domain,
                    'gender', 'aggregation2',
                    'age',    'aggregation3'))
px_domain(x2)
#> # A tibble: 3 × 2
#>   `variable-code` domain      
#>   <chr>           <chr>       
#> 1 gender          aggregation2
#> 2 age             aggregation3
#> 3 year            aggregation1

# Set DOMAIN for individual languages
x3 <-
  x2 %>%
  px_languages(c('en', 'kl')) |>
  px_domain(tribble(~`variable-code`, ~language, ~domain,
                    'gender',    'en',      'aggregation2_en',
                    'gender',    'kl',      'aggregation2_kl',
                    'age',       'en',      'aggregation3_en'))
px_domain(x3)
#> # A tibble: 3 × 3
#>   `variable-code` language domain         
#>   <chr>           <chr>    <chr>          
#> 1 gender          en       aggregation2_en
#> 2 gender          kl       aggregation2_kl
#> 3 age             en       aggregation3_en

# Remove DOMAIN
x4 <- px_domain(x3, NULL)
px_domain(x4)
#> NULL
```
