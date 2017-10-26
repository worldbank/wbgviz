library(wbstats)

wdi_regions <- c("ZG","8S","Z4","Z7","ZJ","XU","ZQ")

wdi_ind <- wbstats::wb_cachelist$indicators
wdi_source <- function(indicators) {
  core_indicators <- setdiff(indicators, c('SP.POP.TOTL', 'SP.RUR.TOTL', 'SP.URB.TOTL'))
  if (length(core_indicators) == 0) {
    core_indicators <- indicators
  }
  orgs <- wdi_ind$sourceOrg[wdi_ind$indicatorID %in% core_indicators]
  orgs <- unique(orgs)
  source <- paste0(
    paste(orgs, collapse="; "), " ",
    "WDI (", paste(indicators, collapse="; "), ")."
  )
  source
}