# Create a px object

Create a px object from a PX-file, an Excel metadata workbook, or a data
frame.

## Usage

``` r
px(input = NULL, data = NULL, validate = TRUE)
```

## Arguments

- input:

  Optional character string. Can be:

  - Path to a PX-file

  - Path to an Excel metadata workbook

  - A data frame

  - Path to an `.rds` or `.parquet` file with a data frame

  - Direct download URL of a PX-file

  - Direct download URL of a parquet file

  If input is a data frame or NULL, a px object with minimal metadata is
  created.

- data:

  Either a data frame or a path to an `.rds` or `.parquet` file with a
  data frame. This can only be used if `input` is an Excel metadata
  workbook.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object

## Examples

``` r
# Create px object from dataset
x1 <- px(population_gl)

# Download PX-file for example
download_succeeded <- tryCatch({
  px_path <- tempfile(fileext = ".px")
  url <- "https://bank.stat.gl:443/sq/0cf06962-19f1-4d5c-8d43-b7ed0009617d"
  download.file(url, px_path)
  TRUE
}, error = function(e) FALSE)

# Run examples only if file was downloaded
if (download_succeeded) {
  # Create px object from PX-file
  x2 <- px(px_path)

  # Create px object from URL
  x3 <- px(url)
}

# Create minimal px object
x4 <- px()
```
