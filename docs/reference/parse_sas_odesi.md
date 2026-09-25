# Parse an ODESI-generated SAS setup file

The Borealis/ODESI deposits of older StatCan PUMFs (e.g. the monthly
Labour Force Survey 1976-2005, catalogue 71M0001XCB) ship one
machine-generated SAS program per language:

## Usage

``` r
parse_sas_odesi(eng_path, fra_path = NULL, encoding = "CP1252")
```

## Arguments

- eng_path:

  English SAS program.

- fra_path:

  Optional French SAS program for the same dataset.

- encoding:

  Encoding of the programs (ODESI writes Windows-1252).

## Value

\`list(variables, codes, layout)\` in the canonical schema. Variables
with a value format are typed \`"character"\`, the rest \`"numeric"\`.

## Details

“\` PROC FORMAT LIBRARY=LIBRARY ; Value V4_F 1='Employed, at work' ;
DATA OUT.x; INFILE 'x.txt' LRECL = 134; INPUT REC_NUM 1-5 SURVYEAR 6-9
... FWEIGHT 131-134 ; FORMAT LFSSTAT V4_F. ; LABEL LFSSTAT='Respondent
Labour Force Status' “\`

Formats are tied to variables by the \`FORMAT var fmt.\` statements, so
a French program (same format names, same variables) supplies
\`label_fr\` by position-independent \`(name, val)\` matching.
