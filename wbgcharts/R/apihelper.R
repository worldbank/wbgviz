library(dplyr)

wb_newcache <- wbstats::wbcache()

wbgref <- list()

countries_df <- wb_newcache$countries %>%
  filter(region != "Aggregates")

wbgref$countries <- list(
  iso2c = countries_df$iso2c,
  iso3c = countries_df$iso3c,
  labels = setNames(countries_df$country, countries_df$iso3c),
  iso2to3 = countries_df %>% select(iso2c, iso3c),
  regions = countries_df %>% select(iso3c, region_iso3c = regionID)
)

regions_df <- wb_newcache$countries %>%
  filter(iso3c %in% c("EAS","ECS","LCN","MEA","NAC","SAS","SSF"))

wbgref$regions <- list(
  iso2c = regions_df$iso2c,
  iso3c = regions_df$iso3c,
  labels = setNames(regions_df$country, regions_df$iso3c),
  iso2to3 = regions_df[,c("iso2c", "iso3c")]
)

wbgref$all_geo <- list(
  iso2c = wb_newcache$countries$iso2c,
  iso3c = wb_newcache$countries$iso3c,
  labels = setNames(wb_newcache$countries$country, wb_newcache$countries$iso3c),
  iso2to3 = wb_newcache$countries[,c("iso2c", "iso3c")]
)

#' @import wbstats
#' @import magrittr
#' @import tidyr
#' @export
wbgdata <-function(country = "all", col.indicator = FALSE, cache = wbgcharts::wb_newcache, ..., indicator.wide = TRUE, removeNA = FALSE) {
  df <- wbstats::wb(country, removeNA = removeNA, cache = cache,...)

  df <- df %>% left_join(wbgref$all_geo$iso2to3, by = "iso2c")
  if (!col.indicator)
    df <- df %>% select(-indicator)
  df <- df %>% select(-iso2c, -country)
  df <- df %>% mutate(date = as.numeric(date))

  if (indicator.wide) {
    df <- df %>% tidyr::spread("indicatorID", "value")
  }

  df
}

wdi_ind <- wb_newcache$indicators

#' @export
wbg_source <- function(indicatorIDs) {
  core_indicators <- setdiff(indicatorIDs, c('SP.POP.TOTL', 'SP.RUR.TOTL', 'SP.URB.TOTL'))
  core_indicators <- core_indicators[!grepl('SP\\.POP', core_indicators)]
  if (length(core_indicators) == 0) {
    core_indicators <- indicatorIDs
  }
  orgs <- wdi_ind$sourceOrg[wdi_ind$indicatorID %in% core_indicators]
  orgs <- unique(orgs)
  orgs <- unlist(lapply(strsplit(orgs, "\\.( [A-Z]|$)"), first))
  orgs <- gsub("\\s*\\([^\\)]+\\)","", orgs)
  sources <- paste0(
    paste(orgs, collapse="; "), ". ",
    "WDI (", paste(indicatorIDs, collapse="; "), ")."
  )
  return(sources)
}

#' @export
wbg_name <- function(indicatorID) {
  return(wdi_ind$indicator[wdi_ind$indicatorID == indicatorID])
}

#save(wbgref, file="./data/wbgref.rda")
#save(wb_newcache, file="./data/wb_newcache.rda")
