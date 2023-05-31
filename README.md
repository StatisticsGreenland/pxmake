# pxmake

![R-CMD-check](https://github.com/StatisticsGreenland/pxmake/actions/workflows/R-CMD-check.yml/badge.svg)

Functions and templates to convert a tidy dataset into a PX file, combining necessary metadata of both technical and informational kind to data.

## Installation

Install the latest release of 'pxmake' by running:

```r
# install.packages('devtools')
# Latest release
devtools::install_github('StatisticsGreenland/pxmake/@*release')
```

Install the development version by running:

```r
# install.packages('devtools')
# Development version
devtools::install_github('StatisticsGreenland/pxmake')
```


'pxmake' is not available on CRAN.

## PX specification

See the [PX-file format specification on Statistics Swedens homepage](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf).

## Developers

### PxJob
Some tests cases uses [PxJob](https://www.stat.fi/tup/tilastotietokannat/px-tuoteperhe_en.html). Install [pxjob64Win](https://github.com/StatisticsGreenland/pxjob64Win) to be able ro run these tests. This only works on Windows.

### How to create a new release
1. Run `usethis::use_version('major'/'minor'/'patch')`. Answer 'No' to the prompt 'Is it ok to commit them?'.
1. Update NEWS.md with all changes since last release.
1. Commit changes with message 'Increment version number to X.Y.Z'.
1. Run `git tag vX.Y.Z`.
1. Run `git push`.
1. Run `git push --tags`.
1. Run `devtools::use_github_release()`.
1. Review relase description. If sattified, click the 'edit' pen in the upper right corner.
1. Click 'Publish release'.
1. Run `usethis::use_version('dev')`. Answer 'Yes' to the prompt 'Is it ok to commit them?'.
1. Run `git push`.
