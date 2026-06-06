get_data_cache_path <- function(pumf_cache_path, pumf_series, pumf_version) {
  cached_pumf <- dir(pumf_cache_path)
  path = NULL
  if (pumf_series=="Census") {
    ylv <- get_year_level_version(pumf_version)
    if (ylv$year=="2021") {
      if (ylv$version=="hierarchical") {
        path <- cached_pumf[grepl("98M0001X",cached_pumf,ignore.case = TRUE)&grepl("cen21_hier",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
      } else {
        path <- cached_pumf[grepl("98M0001X",cached_pumf,ignore.case = TRUE)&grepl("2021|cen21_ind",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
      }
    } else if (ylv$year=="2016"){
      if (ylv$version=="hierarchical") {
        path <- cached_pumf[grepl("98M0002X",cached_pumf,ignore.case = TRUE)&grepl("2016|cen16_hier",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
      } else {
        path <- cached_pumf[grepl("98M0001X",cached_pumf,ignore.case = TRUE)&grepl("2016|cen16_ind",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
      }
    }
  }

  path
}
