# Save px object as an Excel workbook

Save px object as an Excel workbook

## Usage

``` r
save_px_as_xlsx(x, path, save_data, data_path)
```

## Arguments

- x:

  A px object

- path:

  Path to save Excel workbook

- save_data:

  If FALSE, no 'Data' sheet is created in the Excel workbook. Can only
  be used if `path` is an `.xlsx` file.

- data_path:

  Path to an `.rds` or `.parquet` file to save data table at. Can only
  be used if `path` is an `.xlsx` or `.R` file, and `save_data` is
  `TRUE`.

## Value

Nothing
