# Check whether canonical metadata exists for a version directory

Check whether canonical metadata exists for a version directory

## Usage

``` r
metadata_exists(version_dir, bare = FALSE)
```

## Arguments

- version_dir:

  Path to the version directory.

- bare:

  When \`TRUE\`, treat \`version_dir\` as the metadata directory itself
  (used for per-module subdirectories), checking it directly for
  \`variables.csv\` instead of a \`metadata/\` child.

## Value

Logical.
