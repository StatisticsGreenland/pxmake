
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pxmake

<!-- badges: start -->

![R-CMD-check](https://github.com/StatisticsGreenland/pxmake/actions/workflows/R-CMD-check.yml/badge.svg)
<!-- badges: end -->

pxmake is an R package for creating and modifying px files.

With pxmake you can:

- Import a PC-AXIS px file, modify it, and save it as a new px file.
- Save a px file as an Excel workbook.
- Do complex modifications to a px file, like adding total levels to a
  variable.

## Installation

Install the latest version by running:

``` r
# install.packages('devtools')
devtools::install_github('StatisticsGreenland/pxmake')
```

pxmake is not available on CRAN.

## How to use

Use `px()` and `pxsave()` to read/write a px file to a px object.

``` r
library(pxmake)

# Read px file into a px object
x <- px(input = "example.px")

class(x)
#> [1] "px"

# Save px object as a new px file
pxsave(x, path = "example2.px")
```

To modify a px object, use pxmake’s *modifying functions*.

In general, modifying functions are named after the keyword they modify.
It’s possible to chain multiple modifying functions together in
tidyverse style by using the pipe operator `%>%`.

``` r
library(magrittr) # import pipe

# Create px object from data frame
x <- px(data.frame(year = as.character(rep(2021:2023, each = 3)), 
                   group = c('a', 'b', 'c'), 
                   value = runif(9)
                   )
        ) 

head(x$data, 4)
#> # A tibble: 4 × 3
#>   year  group value
#>   <chr> <chr> <dbl>
#> 1 2021  a     0.266
#> 2 2021  b     0.372
#> 3 2021  c     0.573
#> 4 2022  a     0.908

x %>% 
  timeval("year") %>%  # Set year as TIMEVAL
  heading("year") %>%  # Set year as HEADING
  stub("group") %>%    # Set group as STUB
  decimals("2") %>%    # Set DECIMALS to 2
  pxsave("example.px") # Save as px file
```

### Modifying functions

Currently the following 13 keywords have a modifying function in pxmake:

    #>        Keyword Function name
    #>        CHARSET       charset
    #>  CREATION-DATE creation_date
    #>       DECIMALS      decimals
    #>        HEADING       heading
    #>       LANGUAGE      language
    #>      LANGUAGES     languages
    #>   LAST-UPDATED  last_updated
    #>         MATRIX        matrix
    #>    NEXT-UPDATE   next_update
    #>           STUB          stub
    #>        TIMEVAL       timeval
    #>      VALUENOTE     valuenote
    #>     VALUENOTEX    valuenotex

In addition to the above, the following 2 modifying functions are
available:

    #>  Function name
    #>     add_totals
    #>        figures

See the help page for each modifying function for more information.

<details>
<summary>
Keywords without modifying functions
</summary>

These 71 keywords currently doesn’t have a modifying function, but can
be implemented.

    #>              Keyword       Function name Priority Complexity
    #>        AGGREGALLOWED       aggregallowed                Easy
    #>         ATTRIBUTE-ID        attribute_id                    
    #>       ATTRIBUTE-TEXT      attribute_text                    
    #>           ATTRIBUTES          attributes                    
    #>              AUTOPEN             autopen                Easy
    #>         AXIS-VERSION        axis_version       **       Easy
    #>           BASEPERIOD          baseperiod        *       Hard
    #>             CELLNOTE            cellnote        *       Hard
    #>            CELLNOTEX           cellnotex        *       Hard
    #>             CFPRICES            cfprices        *       Easy
    #>             CODEPAGE            codepage       **       Easy
    #>         CONFIDENTIAL        confidential        *       Easy
    #>              CONTACT             contact       **     Medium
    #>             CONTENTS            contents       **     Medium
    #>         CONTVARIABLE        contvariable       **       Hard
    #>            COPYRIGHT           copyright       **       Easy
    #>                 DATA                data       **       Easy
    #>             DATABASE            database                    
    #>             DATANOTE            datanote                    
    #>         DATANOTECELL        datanotecell                    
    #>          DATANOTESUM         datanotesum                    
    #>          DATASYMBOL1         datasymbol1                    
    #>          DATASYMBOL2         datasymbol2                    
    #>          DATASYMBOL3         datasymbol3                    
    #>          DATASYMBOL4         datasymbol4                    
    #>          DATASYMBOL5         datasymbol5                    
    #>          DATASYMBOL6         datasymbol6                    
    #>        DATASYMBOLNIL       datasymbolnil                    
    #>        DATASYMBOLSUM       datasymbolsum                    
    #>               DAYADJ              dayadj                    
    #>        DEFAULT-GRAPH       default_graph                    
    #>          DESCRIPTION         description       **     Medium
    #>   DESCRIPTIONDEFAULT  descriptiondefault                    
    #>       DIRECTORY-PATH      directory_path                    
    #>               DOMAIN              domain       **     Medium
    #>         DOUBLECOLUMN        doublecolumn                    
    #>          ELIMINATION         elimination       **     Medium
    #>      FIRST-PUBLISHED     first_published                    
    #>          HIERARCHIES         hierarchies                    
    #>      HIERARCHYLEVELS     hierarchylevels                    
    #>  HIERARCHYLEVELSOPEN hierarchylevelsopen                    
    #>       HIERARCHYNAMES      hierarchynames                    
    #>                 INFO                info                    
    #>             INFOFILE            infofile                    
    #>                 KEYS                keys                    
    #>                 LINK                link              Medium
    #>                  MAP                 map       **       Easy
    #>              META-ID             meta_id                    
    #>                 NOTE                note       **       Hard
    #>                NOTEX               notex       **       Hard
    #>  OFFICIAL-STATISTICS official_statistics                    
    #>          PARTITIONED         partitioned                    
    #>            PRECISION           precision       **     Medium
    #>             PRESTEXT            prestext                    
    #>            PX-SERVER           px_server                    
    #>            REFPERIOD           refperiod                    
    #>             ROUNDING            rounding                    
    #>              SEASADJ             seasadj                    
    #>         SHOWDECIMALS        showdecimals       **       Easy
    #>               SOURCE              source       **     Medium
    #>              STOCKFA             stockfa       **       Easy
    #>         SUBJECT-AREA        subject_area       **     Medium
    #>         SUBJECT-CODE        subject_code       **       Easy
    #>               SURVEY              survey                    
    #>             SYNONYMS            synonyms                    
    #>              TABLEID             tableid       **       Easy
    #>                TITLE               title              Medium
    #>                UNITS               units       **     Medium
    #>     UPDATE-FREQUENCY    update_frequency       **       Easy
    #>               VALUES              values       **       Hard
    #>        VARIABLE-TYPE       variable_type       **       Hard

Finally these 2 keywords will not have a modifying function, because
they are automatically determined by the data.

    #>       Keyword
    #>         CODES
    #>  VARIABLECODE

</details>

## Need help?

If you have any questions or need help, feel free to [open an
issue](https://github.com/StatisticsGreenland/pxmake/issues/new) on
GitHub, or contact [Lars and
Johan](https://github.com/StatisticsGreenland/pxmake/graphs/contributors)
via email.

## For developers

### PX specification

See the [PX-file format specification on Statistics Swedens
homepage](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf).

### PxJob

Some tests cases uses
[PxJob](https://www.stat.fi/tup/tilastotietokannat/px-tuoteperhe_en.html).
Install [pxjob64Win](https://github.com/StatisticsGreenland/pxjob64Win)
to be able ro run these tests. This only works on Windows.

### How to create a new release

1.  Checkout ‘main’ branch.
2.  Run `usethis::use_version('major'/'minor'/'patch')`. Answer ‘No’ to
    the prompt ‘Is it ok to commit them?’.
3.  Update NEWS.md with all changes since last release.
4.  Commit changes with message ‘Increment version number to X.Y.Z’.
5.  Run `git tag vX.Y.Z`.
6.  Run `git push`.
7.  Run `git push --tags`.
8.  Run `devtools::use_github_release()`.
9.  Review relase description. If sattified, click the ‘edit’ pen in the
    upper right corner.
10. Click ‘Publish release’.
11. Run `usethis::use_version('dev')`. Answer ‘Yes’ to the prompt ‘Is it
    ok to commit them?’.
12. Run `git push`.
