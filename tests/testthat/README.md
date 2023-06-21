Test pxmake.

The 3 main data tables asserts the following features:

Headings
- 1 heading (BEXLTALL, FOTEST)
- 2 headings (BEXSTA)

Languages
- 1 language (FOTEST)
- 2-3 languages (BEXSTA, BEXLTALL)
- Other main language than English (FOTEST)

Data
- Data in 'Data' sheet in Excel (FOTEST)
- Data in .rds file (BEXSTA, BEXLTALL)

Time
- Years (BEXSTA, BEXLTALL)
- Quaters (FOTEST)
- Data without a timeval (no_timeval_or_codes)

Encoding
- utf-8 (BELTALL, BEXSTA, FOTEST)
- Windows-1252 (BEXSTA_windows_1251)

Long lines
- VALUES longer than 256 characters (BEXLTALL)
- NOTE longer than 256 characters (BEXSTA)
#
Other
- >=2 STUBS (BEXSTA, BEXLTALL, FOTEST)
- Data with groups (BEXLTALL)
- Numeric variable type ('age' in BEXLTALL)
- A value in 'Codelist' is not present in the data. (BEXLTALL)
- Data without codes (no_timeval_or_codes)
- Variable names in source data are preserved by using VARIABLECODE (FOTEST)
