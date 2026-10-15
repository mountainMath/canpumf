/* Synthetic fixture - Census 2011 (individuals) style: VALUE LABELS blocks   */
/* headed by an undeclared name with a trailing underscore ("MOB1_"), plus a */
/* genuine variable that ends in "_" and must be left alone.                 */

DATA LIST FILE=DATA/
   MOB1 1
   PKID0_1 2
   FLAG_ 3
   SEX 4
   .

VARIABLE LABELS
   MOB1 'Mobility status - Place of residence 1 year ago'
   PKID0_1 'Presence of children aged 0 to 1'
   FLAG_ 'A variable whose name really ends in underscore'
   SEX 'Sex'
   .

VALUE LABELS
/MOB1_
   1 "Non-movers"
   2 "Movers"
   8 "Not available"
   9 "Not applicable"
/PKID0_1_
   0 "None"
   1 "One or more"
   8 "Not available"
   9 "Not applicable"
/FLAG_
   0 "No"
   1 "Yes"
/SEX
   1 "Female"
   2 "Male"
   .
