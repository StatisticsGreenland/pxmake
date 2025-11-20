# NOTEX

Inspect or change NOTEX.

## Usage

``` r
px_notex(x, value, validate)

# S3 method for class 'px'
px_notex(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string, a data frame, or a list.

  - Use character, to set NOTEX for the entire table across all
    languages.

  - Use a data frame with columns 'language' and 'value' to set NOTEX
    for the entire table in a specific language.

  - Use a data frame with the columns 'variable-code' and 'notex', to
    set NOTEX for a specific variable across all languages. Add the
    column 'language' to set NOTEX for specific language.

  - Use a list of the above elements to set NOTEX in muliple ways. This
    is the same as calling NOTEX multiple times with different values.

  - If missing, the current NOTEX is returned.

  - If NULL, NOTEX is removed for the table and all variables.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object, a character string, a data frame, or a list of character
strings and/or data frames.

## Details

NOTEX has a lot of possible ways to specify `value`, because it can be
set both for the entire PX-file and for individual variables.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A50%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C271%2C0%5D)

## Examples

``` r
library(tibble)

# Set NOTEX for entire PX-file
x1 <-
  px(population_gl) |>
  px_notex('Note about PX-file')

# Print NOTEX
px_notex(x1)
#> [1] "Note about PX-file"

# Set NOTEX for entire PX-file in multiple languages
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_notex(tribble(~language,            ~value,
                       'en',    'English notex',
                       'kl', 'Kalaallisut notex'
                 )
         )
px_notex(x2)
#> # A tibble: 2 × 2
#>   language value            
#>   <chr>    <chr>            
#> 1 en       English notex    
#> 2 kl       Kalaallisut notex

# Set NOTEX for variables
x3 <-
  x1 |>
  px_notex(tribble(~`variable-code`, ~notex,
                 'year', 'Some data collected in following year',
                 'age',  'Is rounded down'
                 )
        )
px_notex(x3)
#> [[1]]
#> [1] "Note about PX-file"
#> 
#> [[2]]
#> # A tibble: 2 × 2
#>   `variable-code` notex                                
#>   <chr>           <chr>                                
#> 1 age             Is rounded down                      
#> 2 year            Some data collected in following year
#> 

# Remove all NOTEXs
x4 <- px_notex(x3, NULL)
```
