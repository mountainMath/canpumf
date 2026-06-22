# Build a labeled DuckDB table for a PUMF version

Reads the canonical metadata from \`metadata/\`, reads the raw data
file, optionally joins bootstrap weights, applies code labels as
factors, converts numeric columns, and writes to a \`.duckdb\` file.

## Usage

``` r
pumf_build_duckdb(
  version_dir,
  series,
  version,
  lang = "eng",
  layout_mask = NULL,
  file_mask = NULL,
  refresh = FALSE,
  db_path = NULL,
  meta_subdir = NULL,
  data_fixups = NULL,
  bsw_override = NULL
)
```

## Arguments

- version_dir:

  Path returned by \[pumf_locate_or_download()\].

- series:

  Survey series acronym, e.g. \`"SFS"\`.

- version:

  Version string, e.g. \`"2019"\`.

- lang:

  \`"eng"\` (default) or \`"fra"\`.

- layout_mask:

  Optional layout mask; used in the DuckDB table name.

- file_mask:

  Optional regex to select the data file. Overrides registry.

- refresh:

  If \`TRUE\`, drop and rewrite the DuckDB table.

- db_path:

  Optional explicit path to the DuckDB file. Defaults to
  \`\<version_dir\>/\<series\>\_\<version\>.duckdb\`. Multi-module
  surveys pass one shared path so every module table lands in the same
  file.

- meta_subdir:

  Optional metadata subdirectory under \`metadata/\` to read for this
  build. \`NULL\` (default) uses \`metadata/\` (the primary module);
  secondary modules pass their module id so \`metadata/\<id\>/\` is
  read.

- data_fixups:

  Optional \`data_fixups\` list overriding the registry entry's for this
  build. Used by secondary modules, which supply their own complete
  fixup set (e.g. \`force_numeric\`), replacing the entry's primary
  fixups.

- bsw_override:

  Optional list of bootstrap-weight config (\`bsw_mask\`,
  \`bsw_file_mask\`, \`bsw_join_key\`, \`bsw_drop_cols\`,
  \`bsw_strata\`) overriding the registry entry's BSW config for this
  build. Each module of a multi-module survey joins its own bootstrap
  weights (or none), so the caller passes that module's BSW config; an
  override whose fields are all \`NULL\` means "this module has no
  bootstrap weights".

## Value

Invisibly, a named list with \`db_path\` and \`table_name\`.

## Details

Skips re-building if the named table already exists in the DuckDB file
and \`refresh = FALSE\`. Passing \`refresh = TRUE\` drops and rewrites
the table without re-downloading or re-extracting raw data.

Returns the db path and table name invisibly. Call
\[pumf_open_duckdb()\] to open a read-only connection and get a lazy
\`dplyr::tbl()\`. Keeping Stage 3 and connection-opening separate
prevents DuckDB file-lock conflicts when building multiple language
tables for the same survey in one session.
