# Multi-module surveys (linked files in one DuckDB)

Back to [CLAUDE.md](../CLAUDE.md). User-facing docs: `vignettes/submodules.Rmd`.

Some surveys ship several linked files that share a respondent key and have to be joined for analysis. Each is modelled as **several tables in one DuckDB file**, joinable on the shared key. They are *not* separate databases, which could not be joined on a single connection.

| Registry key | Modules (primary first) | `module_key` |
|---|---|---|
| GSS `Cycle 16 (2002)` (Aging and Social Support) | MAIN, CG4, CG6, CR | `RECID` (person weight `WGHT_PER` only in MAIN) |
| GSS `Cycle 12 (1998)`, `Cycle 24 (2010)` (Time Use) | Main, Episode | `RECID` |
| GSS `Cycle 29 (2015)`, `Cycle 36 (2022)` (Time Use) | Main, Episode | `PUMFID` |
| SHS `2017` | Interview, Diary | `CASEID` |
| SGVP `2004`, `2007`, `2010` | MAIN, GS | `PUMFID` |
| SGVP `2000` | MAIN, GS, VD | `MICRO_ID` |
| SGVP `1997` | MAIN, GIVE, VOLNTR | `IDNUM` |

## Registry

- `.make_entry(modules = list(MAIN = ..., CG4 = ...), module_key = ...)`. Each module has its own `layout_mask`, `file_mask`, `data_fixups` and BSW config (`bsw_mask`, `bsw_file_mask`, `bsw_join_key`, `bsw_drop_cols`, `bsw_strata`).
- One module is `primary`. Its config is copied to the entry's top level, so the single-table code paths, `.read_bsw_data(reg)` and the override ledger all see it.
- `.pumf_entry_modules(reg)` returns the per-module config: `list(id, layout_mask, file_mask, data_fixups, bsw_*, is_primary, meta_subdir)`. The primary module's `meta_subdir` is `NULL`, so it uses `metadata/`. Secondary modules use `metadata/<id>/`.
- `.pumf_module_key(reg)` returns the shared key, which is recorded once per entry.

## Pipeline

`pumf_run_pipeline()` loops over the modules, running Stage 2 (`pumf_parse_metadata(..., layout_mask, meta_subdir)`) and Stage 3 (`pumf_build_duckdb(..., layout_mask, file_mask, meta_subdir, data_fixups, bsw_override)`) for each. All tables land in the one DuckDB file, and the primary module's tbl is returned.

Each module joins its **own** bootstrap weights through `bsw_override`, so the Interview replicate weights are not mis-joined onto Diary. An override whose fields are all `NULL` means "this module has no BSW". Table names come from `.pumf_table_name(series, version, lang, module)` (`<lang>_<layout_mask>`), so each module gets a distinct table.

## API

- `get_pumf("GSS", "Cycle 16 (2002)")` returns the primary table. `.pumf_announce_modules()` emits a one-time message listing the sibling modules, with a `pumf_module()` example.
- `get_pumf(..., module = "CG4")` opens a module standalone, on its own connection.
- `pumf_module(tbl, "CG4")` opens a sibling module **on the same connection**, so the two can be joined. The first call per survey announces the `module_key`.
- `label_pumf_columns()`/`pumf_var_labels()` are module-aware. `.pumf_tbl_module()` recovers the module from the tbl's remote table name, falling back to the module registered for the connection after joins. Each module therefore reads its own `metadata/<id>/variables.csv`, even though all modules share one connection.
