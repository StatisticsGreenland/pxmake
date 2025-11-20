# Split long strings at commas

Long strings are split so they are no longer than 256 characters and end
at a comma.

## Usage

``` r
break_long_lines(str, max_line_length = 256)
```

## Arguments

- str:

  String

- max_line_length:

  Integer longest allowed line length

## Value

A character vector
