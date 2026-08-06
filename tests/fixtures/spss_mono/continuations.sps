/* Synthetic SPSS monolithic fixture - string continuations in every form.
   StatCan keeps long labels inside the file's line width with SPSS's string
   concatenation operator, and across the Census 2021 command files alone it
   appears in all four combinations of quote character and line break. */

DATA LIST FILE=DATA/
   V1 1-2
   V2 3-4
   V3 5-6
   V4 7-8
   V5 9-10
   .

VARIABLE LABELS
   V1 'Single quoted, ' +
      'broken across lines'
   V2 "Double quoted, " +
      "broken across lines"
   V3 "Double quoted, " + "joined inline"
   V4 "Mixed delimiters, ending in d'eq" +
      'uivalence'
   V5 'Three ' +
      'fragments ' +
      'chained'
   .

VALUE LABELS
/V1
   1 'Code label, ' +
     'continued'
   2 "Plain"
   .
