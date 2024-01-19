# pxmake - Create px-files in R

![R-CMD-check](https://github.com/StatisticsGreenland/pxmake/actions/workflows/R-CMD-check.yml/badge.svg)

## Overview

`pxmake` is an R package for creating and modifying px-files.

-   You can import any\* PC-AXIS px-file, modify it, and save it as a new px-file.
-   You can create a new px-file from scratch, by starting with a data frame.
-   Save a px-file in an Excel Workbook.

\**that is the goal at least*

## Installation

Install the latest version by running:

``` r
# install.packages('devtools')
devtools::install_github('StatisticsGreenland/pxmake')
```

`pxmake` is not available on CRAN.

## How to use

Use `px()` to import a px-file and convert it into a px object.

``` r
library(pxmake)

x <- px(input = "path/to/px-file.px")

class(x)
#> [1] "px"
```

The px object is a list of 8 data frames. The first 7 contain metadata, and the last is the data table.

``` r
> names(x)
[1] "languages"  "table1"     "table2"     "variables1" "variables2" "codelists1" "codelists2" "data"
```

The structure of the 7 metadata tables are hopefully intuitive, if you are familiar the px file format.

Use `pxsave()` to save the px object.

``` r
pxsave(x, path = "path/to/new-px-file.px")
```

### Excel workbook
Apart from px-files, `pxmake` can also read and write Excel workbooks. This allows users who aren't familiar with the px file to edit that content, and convert it back to a px file.

``` r
# Convert a px-file to an Excel workbook
x <- px("path/to/px-file.px")

pxsave(x, path = "path/to/excel-file.xlsx")

# Open and modify the Excel workbook, then convert it back into a px-file
x <- px("path/to/excel-file.xlsx")

pxsave(x, path = "path/to/new-px-file.px"")
```

### Add totals
Variables in px-files commenly contain 'total' levels, which is a sum of all other levels in that variable. `pxmake` can add these totals automatically.

``` r
x <- px("path/to/px-file.px")

x <- add_totals(x, vars = c("variable1", "variable2"))
```
See `?add_totals` for more information.

## For developers

### PX specification

See the [PX-file format specification on Statistics Swedens homepage](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf).

### PxJob

Some tests cases uses [PxJob](https://www.stat.fi/tup/tilastotietokannat/px-tuoteperhe_en.html). Install [pxjob64Win](https://github.com/StatisticsGreenland/pxjob64Win) to be able ro run these tests. This only works on Windows.

### How to create a new release

1.  Checkout 'main' branch.
2.  Run `usethis::use_version('major'/'minor'/'patch')`. Answer 'No' to the prompt 'Is it ok to commit them?'.
3.  Update NEWS.md with all changes since last release.
4.  Commit changes with message 'Increment version number to X.Y.Z'.
5.  Run `git tag vX.Y.Z`.
6.  Run `git push`.
7.  Run `git push --tags`.
8.  Run `devtools::use_github_release()`.
9.  Review relase description. If sattified, click the 'edit' pen in the upper right corner.
10. Click 'Publish release'.
11. Run `usethis::use_version('dev')`. Answer 'Yes' to the prompt 'Is it ok to commit them?'.
12. Run `git push`.
