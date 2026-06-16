/* Synthetic SPSS monolithic fixture - 2016 style (double quotes, / on own line) */

DATA LIST FILE=DATA/
   AGEGRP 1-2   PROV 3-4   WAGE 5-10   SEX 11 .

FORMATS
   AGEGRP (F2.0) / PROV (A2) / WAGE (F6.2) / SEX (F1.0) / .

VARIABLE LABELS
AGEGRP "Age group"
PROV "Province of residence"
WAGE "Total employment income"
SEX "Sex"
.

VALUE LABELS
AGEGRP
 1 "Under 25 years"
 2 "25 to 34 years"
 3 "35 to 44 years"
 99 "Not available"
/
PROV
 10 "Newfoundland and Labrador"
 11 "Prince Edward Island"
 12 "Nova Scotia"
 24 "Quebec"
 35 "Ontario"
/
SEX
 1 "Male"
 2 "Female"
 9 "Not available"
.

MISSING VALUES
WAGE (9999999)
AGEGRP (99)
.
