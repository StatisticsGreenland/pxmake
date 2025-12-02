# Create micro PX-files

Split one px object into many small PX-files (micro files), with count
of the variables in it.

## Usage

``` r
px_micro(x, out_dir = NULL, keyword_values = NULL)
```

## Arguments

- x:

  A px object.

- out_dir:

  Directory to save PX-files in.

- keyword_values:

  Optional. A data frame with column 'variable' and one or more of:
  'px_contents', 'px_title', 'px_description', and 'px_matrix'. The
  columns will be added as keywords to the table for each non-HEADING
  variable that match the 'variable' column. It probably work for other
  keywords as well.

  Use the column 'filename' to control the filename of each micro file.
  The filename path is relative to 'out_dir'.

  Use the column 'language' if the PX-file has multiple languages.

## Value

Nothing

## Details

The HEADING variables are use in all the micro files, and a file is
created for each non-HEADING variable. The new PX-files are saved in a
directory specified by `out_dir`.

The main loop uses the furrr package for parallelisation. Use
future::plan() to choose how to parallelise.

## Examples

``` r
# Create px object with cohort as HEADING
x <-
  greenlanders |>
  px() |>
  px_stub(names(greenlanders)) |>
  px_heading("cohort")

# Create micro files, one for each of the non-HEADING variables (gender, age,
# municipality)
px_micro(x)
#> [1] "Created PX-files in: /tmp/Rtmpj9Ysxy/file1878226b96c3"
```
