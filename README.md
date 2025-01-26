
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pxmake <a href="https://statisticsgreenland.github.io/pxmake/"><img src="man/figures/logo.png" align="right" height="139" alt="pxmake website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/pxmake)](https://cran.r-project.org/package=pxmake)
![R-CMD-check](https://github.com/StatisticsGreenland/pxmake/actions/workflows/R-CMD-check.yml/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/StatisticsGreenland/pxmake/branch/main/graph/badge.svg)](https://app.codecov.io/gh/StatisticsGreenland/pxmake?branch=main)
[![Total
downloads](https://cranlogs.r-pkg.org/badges/last-month/pxmake)](https://cran.r-project.org/package=pxmake)
<!-- badges: end -->

## Overview

‘pxmake’ is an R package for creating and modifying PX-files.

With pxmake you can:

- Import a PX-file, modify it, and save it as a new PX-file.
- Modify all metadata keywords in a PX-file.
- Do complex modifications to a PX-file, like adding total levels to a
  variable.
- Save a PX-file as an Excel workbook.

## Installation

``` r
# Install the latest release from CRAN
install.packages('pxmake')

# Or the development version from GitHub
# install.packages('pak')
pak::pak("StatisticsGreenland/pxmake")
```

## How to use

*(Find complete documentation on [pxmake
webpage](https://statisticsgreenland.github.io/pxmake/).)*

Use `px()` to import an existing PX-file into R.

``` r
library(pxmake)

# Import PX-file
x <- px(input = "example.px")
```

Once imported, use one of pxmake’s *modifying functions*.

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
  px_timeval("year") %>%  # Set year as TIMEVAL
  px_heading("year") %>%  # Set year as HEADING
  px_stub("group") %>%    # Set group as STUB
  px_decimals("2") %>%    # Set DECIMALS to 2
  px_save("example.px") # Save as PX-file
```

### Modifying functions

Currently the following 48 keywords have a modifying function in pxmake:

    #>  Keyword            Function name        
    #>  AGGREGALLOWED      px_aggregallowed     
    #>  AUTOPEN            px_autopen           
    #>  AXIS-VERSION       px_axis_version      
    #>  BASEPERIOD         px_baseperiod        
    #>  CELLNOTE           px_cellnote          
    #>  CELLNOTEX          px_cellnotex         
    #>  CFPRICES           px_cfprices          
    #>  CHARSET            px_charset           
    #>  CODEPAGE           px_codepage          
    #>  CONFIDENTIAL       px_confidential      
    #>  CONTACT            px_contact           
    #>  CONTENTS           px_contents          
    #>  CONTVARIABLE       px_contvariable      
    #>  COPYRIGHT          px_copyright         
    #>  CREATION-DATE      px_creation_date     
    #>  DATA               px_data              
    #>  DECIMALS           px_decimals          
    #>  DESCRIPTION        px_description       
    #>  DESCRIPTIONDEFAULT px_descriptiondefault
    #>  DOMAIN             px_domain            
    #>  ELIMINATION        px_elimination       
    #>  HEADING            px_heading           
    #>  INFOFILE           px_infofile          
    #>  LANGUAGE           px_language          
    #>  LANGUAGES          px_languages         
    #>  LAST-UPDATED       px_last_updated      
    #>  LINK               px_link              
    #>  MAP                px_map               
    #>  MATRIX             px_matrix            
    #>  NEXT-UPDATE        px_next_update       
    #>  NOTE               px_note              
    #>  NOTEX              px_notex             
    #>  PRECISION          px_precision         
    #>  SHOWDECIMALS       px_showdecimals      
    #>  SOURCE             px_source            
    #>  STOCKFA            px_stockfa           
    #>  STUB               px_stub              
    #>  SUBJECT-AREA       px_subject_area      
    #>  SUBJECT-CODE       px_subject_code      
    #>  TABLEID            px_tableid           
    #>  TIMEVAL            px_timeval           
    #>  TITLE              px_title             
    #>  UNITS              px_units             
    #>  UPDATE-FREQUENCY   px_update_frequency  
    #>  VALUENOTE          px_valuenote         
    #>  VALUENOTEX         px_valuenotex        
    #>  VALUES             px_values            
    #>  VARIABLE-TYPE      px_variable_type

In addition to the above, the following 4 modifying functions are
available:

    #>  Function name    
    #>  px_add_totals    
    #>  px_figures       
    #>  px_order         
    #>  px_variable_label

See the help page for each modifying function for more information.

<details>
<summary>
Keywords without modifying functions
</summary>

These 36 keywords currently doesn’t have a modifying function, but can
be implemented.

    #>  Keyword             Function name          Priority Complexity
    #>  ATTRIBUTE-ID        px_attribute_id                           
    #>  ATTRIBUTE-TEXT      px_attribute_text                         
    #>  ATTRIBUTES          px_attributes                             
    #>  DATABASE            px_database                               
    #>  DATANOTE            px_datanote                               
    #>  DATANOTECELL        px_datanotecell                           
    #>  DATANOTESUM         px_datanotesum                            
    #>  DATASYMBOL1         px_datasymbol1                            
    #>  DATASYMBOL2         px_datasymbol2                            
    #>  DATASYMBOL3         px_datasymbol3                            
    #>  DATASYMBOL4         px_datasymbol4                            
    #>  DATASYMBOL5         px_datasymbol5                            
    #>  DATASYMBOL6         px_datasymbol6                            
    #>  DATASYMBOLNIL       px_datasymbolnil                          
    #>  DATASYMBOLSUM       px_datasymbolsum                          
    #>  DAYADJ              px_dayadj                                 
    #>  DEFAULT-GRAPH       px_default_graph                          
    #>  DIRECTORY-PATH      px_directory_path                         
    #>  DOUBLECOLUMN        px_doublecolumn                           
    #>  FIRST-PUBLISHED     px_first_published                        
    #>  HIERARCHIES         px_hierarchies                            
    #>  HIERARCHYLEVELS     px_hierarchylevels                        
    #>  HIERARCHYLEVELSOPEN px_hierarchylevelsopen                    
    #>  HIERARCHYNAMES      px_hierarchynames                         
    #>  INFO                px_info                                   
    #>  KEYS                px_keys                                   
    #>  META-ID             px_meta_id                                
    #>  OFFICIAL-STATISTICS px_official_statistics                    
    #>  PARTITIONED         px_partitioned                            
    #>  PRESTEXT            px_prestext                               
    #>  PX-SERVER           px_px_server                              
    #>  REFPERIOD           px_refperiod                              
    #>  ROUNDING            px_rounding                               
    #>  SEASADJ             px_seasadj                                
    #>  SURVEY              px_survey                                 
    #>  SYNONYMS            px_synonyms

Finally these 2 keywords will not have a modifying function, because
they are automatically determined by the data.

    #>  Keyword     
    #>  CODES       
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

pxmake is based on the [PX-file format specification on Statistics
Swedens
homepage](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf).

### PxJob

Some tests cases uses
[PxJob](https://stat.fi/tup/tilastotietokannat/px-tuoteperhe_en.html).
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
9.  Run `usethis::use_version('dev')`. Answer ‘Yes’ to the prompt ‘Is it
    ok to commit them?’.
10. Run `git push`.
