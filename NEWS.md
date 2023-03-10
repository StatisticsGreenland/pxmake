# pxmake (development version)

## Bug fixes
- CODES added for time variables. (#91)
- Data cube is sorted correctly for data with more than one heading variable. (#93)
- Non-figure variables are read as character. (#87, #84, #73)

# pxmake 0.3.0

## New features
- Time values are added correctly the px file.
- If no `source_data_path` is given, `pxmake()` looks for a 'Data' sheet in the
metadata and uses that instead.
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
