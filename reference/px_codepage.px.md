# CODEPAGE

Inspect or change CODEPAGE.

## Usage

``` r
px_codepage(x, value, validate)

# S3 method for class 'px'
px_codepage(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string. If missing, the current CODEPAGE is
  returned. If NULL, CODEPAGE is removed.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or a character string.

## Details

CODEPAGE controls which encoding PX-files are read and stored in. Use
[`iconvlist()`](https://rdrr.io/r/base/iconv.html) to see available
encodings on your system.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A41%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C497%2C0%5D)

[`px_save()`](https://statisticsgreenland.github.io/pxmake/reference/px_save.md)

## Examples

``` r
# Set CODEPAGE
x1 <-
   px(population_gl) |>
   px_codepage('utf-8')

# Print CODEPAGE
px_codepage(x1)
#> [1] "utf-8"

# Remove CODEPAGE
x2 <- px_codepage(x1, NULL)
px_codepage(x2)
#> NULL
```
