# Open a DuckDB table as a lazy dplyr tbl

Opens a connection to the \`.duckdb\` file produced by
\[pumf_build_duckdb()\] and returns a lazy \`dplyr::tbl()\`. Use
\[close_pumf()\] to release the connection when done.

## Usage

``` r
pumf_open_duckdb(db_path, table_name, read_only = TRUE)
```

## Arguments

- db_path:

  Path to the \`.duckdb\` file.

- table_name:

  Name of the table to open.

- read_only:

  Open in read-only mode (default \`TRUE\`). Pass \`FALSE\` to allow
  write operations on the DuckDB file (e.g. to add custom views).

## Value

A lazy \`dplyr::tbl()\`.
