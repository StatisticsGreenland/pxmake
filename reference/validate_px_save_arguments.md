# Check all arguments to px_save()

Check all arguments to px_save()

## Usage

``` r
validate_px_save_arguments(x, path, save_data, data_path)
```

## Arguments

- x:

  A px object.

- path:

  Path to file. The file extension determines the format. Can be:

  - `.px` to save as a PX-file

  - `.xlsx` to save as an Excel workbook

  - `.R` to save an R-script that creates the px object

- save_data:

  If FALSE, no 'Data' sheet is created in the Excel workbook. Can only
  be used if `path` is an `.xlsx` file.

- data_path:

  Path to an `.rds` or `.parquet` file to save data table at. Can only
  be used if `path` is an `.xlsx` or `.R` file, and `save_data` is
  `TRUE`.

## Value

Nothing
