# pxmake (development version)

# pxmake 0.5.0

## Breaking changes

- Rename 'General' metadata sheet to 'Table'. (#85)
- Rename argument `pxmake(source_data_path)` to `source_data` (#71) 
- Rename argument `metamake(out_path)` to `xlsx_path`. (#106)

## New features

- Write and read files in encoding defined by keyword CODEPAGE. (#114, #115)
- Break lines in px-files with values longer than 256 characters. (#113)
- Support other main languages than English. (#54)
- Support any language code (previously only supported: en, da, kl, and fo). (#117)


## Bug fixes and minor improvements

- Add formatting to Excel metadata workbook created by `metamake()`. (#100)
- Add argument `rds_data_path` to `metamake()` to save data as rds file. (#108)
- Let `pxmake()` accept data frame as source data in addition to accepting a
path. (#71)
- Turn off `readLines()` warning for missing EOL character. (#113)
- Rename `.figures` to `figures_` in metadata created by `metamake()`. (#104)
- Allow forward slashes in variables names.
- Let `metamake()` support tables without time variables. (#120)
- Use values as codes in there are no codes in metadata. (#102)


# pxmake 0.4.0

## New features

- Add metamake() which creates an Excel metadata workbook from a px-file. This
is the inverse function of pxmake(). (#68)

## Bug fixes and minor improvements

- CODES added for time variables. (#91)
- Data cube is sorted correctly for data with more than one heading variable. (#93)
- Non-figure variables are read as character. (#87, #84, #73)
- Add keywords AUTOPEN and AGGREGALLOWED
- Allow figures variable to have any name (previously only 'values')

# pxmake 0.3.0

## New features

- Time values are added correctly the px file.
- If no `source_data_path` is given, `pxmake()` looks for a 'Data' sheet in the metadata and uses that instead.
- New argument: add_totals. Choose variable(s) to add a total level to.

## Bug fixes and minor improvements

- Complete data by using values both from codelist and data.
- Lot of small changes to avoid warnings when running R CMD check.
- Add automatic testing on GitHub.
- Move helper functions to own file.

# pxmake 0.2.0

## New features

- Better names for sheets, variables and keywords in Excel metadata workbook.
- Sort keywords in recommended order

## Bug fixes and minor improvements

- Add installation instruction to README.md

# pxmake 0.1.0

## New features

- Add support for more than one HEADING variable

## Bug fixes and minor improvements

- Allow blank fields in metadata (NA is replaced with "")
- PxJob is run as parts of tests
