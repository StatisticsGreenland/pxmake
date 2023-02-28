# pxmake

![R-CMD-check](https://github.com/StatisticsGreenland/pxmake/actions/workflows/r.yml/badge.svg)

Functions and templates to convert a tidy dataset into a PX file, combining necessary metadata of both technical and informational kind to data.

## Installation

Install by running:

```r
# install.packages('devtools')
devtools::install_github('https://github.com/StatisticsGreenland/pxmake')
```

'pxmake' is not available on CRAN.

## PX specification

See the [PX-file format specification on Statistics Swedens homepage](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf).

## Developers

Some tests cases uses [PxJob](https://www.stat.fi/tup/tilastotietokannat/px-tuoteperhe_en.html). Install [pxjob64Win](https://github.com/StatisticsGreenland/pxjob64Win) to be able ro run these tests. This only works on Windows.
