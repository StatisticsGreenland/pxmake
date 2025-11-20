# NOTE

Inspect or change NOTE.

## Usage

``` r
px_note(x, value, validate)

# S3 method for class 'px'
px_note(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string, a data frame, or a list.

  - Use character, to set NOTE for the entire table across all
    languages.

  - Use a data frame with columns 'language' and 'value' to set NOTE for
    the entire table in a specific language.

  - Use a data frame with the columns 'variable-code' and 'note', to set
    NOTE for a specific variable across all languages. Add the column
    'language' to set NOTE for specific language.

  - Use a list of the above elements to set NOTE in muliple ways. This
    is the same as calling NOTE multiple times with different values.

  - If missing, the current NOTE is returned.

  - If NULL, NOTE is removed for the table and all variables.

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

NOTE has a lot of possible ways to specify `value`, because it can be
set both for the entire PX-file and for individual variables.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A50%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C411%2C0%5D)

## Examples

``` r
library(tibble)

# Set NOTE for entire PX-file
x1 <-
  px(population_gl) |>
  px_note('Note about PX-file')

# Print NOTE
px_note(x1)
#> [1] "Note about PX-file"

# Set NOTE for entire PX-file in multiple languages
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_note(tribble(~language,            ~value,
                       'en',    'English note',
                       'kl', 'Kalaallisut note'
                 )
         )
px_note(x2)
#> # A tibble: 2 × 2
#>   language value           
#>   <chr>    <chr>           
#> 1 en       English note    
#> 2 kl       Kalaallisut note

# Set NOTE for variables
x3 <-
  x1 |>
  px_note(tribble(~`variable-code`, ~note,
                 'year', 'Some data collected in following year',
                 'age',  'Is rounded down'
                 )
        )
px_note(x3)
#> [[1]]
#> [1] "Note about PX-file"
#> 
#> [[2]]
#> # A tibble: 2 × 2
#>   `variable-code` note                                 
#>   <chr>           <chr>                                
#> 1 age             Is rounded down                      
#> 2 year            Some data collected in following year
#> 

# Remove all NOTEs
x4 <- px_note(x3, NULL)
```
