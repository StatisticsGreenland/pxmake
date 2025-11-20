# Save px object to file

Save px object to file

## Usage

``` r
px_save(x, path, save_data = TRUE, data_path = NULL)
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

## Details

Use
[`px_codepage()`](https://statisticsgreenland.github.io/pxmake/reference/px_codepage.px.md)
to change file encoding.

## See also

[`px_codepage()`](https://statisticsgreenland.github.io/pxmake/reference/px_codepage.px.md)

## Examples

``` r
# Save px object to PX-file
tmp_dir <- tempdir()

x <- px(population_gl)

px_save(x, file.path(tmp_dir, "population.px"))

# Save px object to Excel workbook
px_save(x, file.path(tmp_dir, "population.xlsx"))

# Save px object as R-script that creates the px object
px_save(x, file.path(tmp_dir, "population.R"))
```
