# Check all arguments to px()

Check all arguments to px()

## Usage

``` r
validate_px_arguments(input, data)
```

## Arguments

- input:

  Optional character string. Can be:

  - Path to a PX-file

  - Path to an Excel metadata workbook

  - A data frame

  - Path to an `.rds` or `.parquet` file with a data frame

  - URL of a PX-file

  If input is a data frame or NULL, a px object with minimal metadata is
  created.

- data:

  Either a data frame or a path to an `.rds` or `.parquet` file with a
  data frame. This can only be used if `input` is an Excel metadata
  workbook.

## Value

Nothing
