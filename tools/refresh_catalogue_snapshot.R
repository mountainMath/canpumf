# Regenerate the catalogue snapshot shipped with the package.
#
# canpumf ships a frozen copy of the full StatCan PUMF catalogue crawl at
# inst/extdata/pumf_catalogue.rds.  It is the terminal fallback for the
# download-URL resolver (.pumf_resolve_collection_row) and for
# list_statcan_pumf_catalogue() when StatCan is unreachable: a freshly installed
# package with no user cache and no network still resolves every supported
# survey's download URL from this ground-truth copy, so a StatCan markup change
# cannot silently break get_pumf() between releases.
#
# Run this before each release so the shipped snapshot stays current:
#
#   Rscript tools/refresh_catalogue_snapshot.R
#
# It performs a full live crawl (hundreds of requests, a few minutes) and
# overwrites inst/extdata/pumf_catalogue.rds with the list(fetched, prefer, data)
# object the cache readers expect.  This file is .Rbuildignore'd (tools/).

suppressMessages(devtools::load_all(quiet = TRUE))

prefer <- names(canpumf:::.statcan_format_tokens)

message("Crawling the full StatCan PUMF catalogue (this takes a few minutes) ...")
data <- list_statcan_pumf_catalogue(prefer = prefer, refresh = TRUE,
                                    verbose = TRUE)

snapshot <- list(fetched = Sys.time(), prefer = prefer, data = data)

out <- file.path("inst", "extdata", "pumf_catalogue.rds")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
saveRDS(snapshot, out)

# Quick sanity check: the adapter must yield every supported series from the
# fresh snapshot, with no missing download URLs.
adon <- canpumf:::.statcan_catalogue_to_collection(data)
message(sprintf("Wrote %s: %d crawl rows, %d resolvable (Acronym, Version) pairs across %d series.",
                out, nrow(data), nrow(adon), length(unique(adon$Acronym))))
miss <- setdiff(canpumf:::.statcan_supported_series, unique(adon$Acronym))
if (length(miss))
  warning("No catalogue rows for supported series: ", paste(miss, collapse = ", "),
          call. = FALSE)
print(as.data.frame(table(adon$Acronym)))
