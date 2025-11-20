# MAP

Inspect or change MAP.

## Usage

``` r
px_map(x, value, validate)

# S3 method for class 'px'
px_map(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string or data frame.

  - Use character to set MAP for all languages and STUB/HEADING
    variables.

  - Use data frame with columns 'variable-code', 'language' and 'map' to
    set MAP for specific variables.

  - If missing, the current MAP is returned.

  - If NULL, MAP is removed for all variables.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A49%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C354%2C0%5D)

## Examples

``` r
# Set MAP for all languages
x1 <-
  px(population_gl) |>
  px_map('greenland')

# Print MAP
px_map(x1)
#> # A tibble: 3 × 2
#>   `variable-code` map      
#>   <chr>           <chr>    
#> 1 gender          greenland
#> 2 age             greenland
#> 3 year            greenland

# Set MAP for individual variables
library(tibble)
x2 <-
  x1 |>
  px_map(tribble(~`variable-code`, ~map,
                    'gender', 'cities',
                    'age',    'municipalities'))
px_map(x2)
#> # A tibble: 3 × 2
#>   `variable-code` map           
#>   <chr>           <chr>         
#> 1 gender          cities        
#> 2 age             municipalities
#> 3 year            greenland     

# Set MAP for individual languages
x3 <-
  x2 %>%
  px_languages(c('en', 'kl')) |>
  px_map(tribble(~`variable-code`, ~language, ~map,
                    'gender',    'en',      'cities_en',
                    'gender',    'kl',      'cities_kl',
                    'age',       'en',      'municipalities_en'))
px_map(x3)
#> # A tibble: 3 × 3
#>   `variable-code` language map              
#>   <chr>           <chr>    <chr>            
#> 1 gender          en       cities_en        
#> 2 gender          kl       cities_kl        
#> 3 age             en       municipalities_en

# Remove MAP
x4 <- px_map(x3, NULL)
px_map(x4)
#> NULL
```
