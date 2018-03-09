#wb_newcache <- wbstats::wbcache()

REFRESH_PERIOD_DAYS <- 30

wburls.v2 <- function ()
{
  base_url <- "http://api.worldbank.org/v2/"
  utils_url <- "per_page=20000&format=json"
  url_list <- list(base_url = base_url, utils_url = utils_url)
  url_list
}

#' @export
get_wbcache <- function(cachedir = rappdirs::user_cache_dir("wbgcharts")) {
  msg_to_update <- "Countries/series cache is stale - use refresh_wbcache() to update."
  filename <- file.path(cachedir, "wbcache.RData")
  if (!file.exists(filename)) {
    warning(msg_to_update)
    wbstats::wb_cachelist
  } else if (difftime(Sys.time(), file.info(filename)$mtime, units = "days") > REFRESH_PERIOD_DAYS) {
    warning(msg_to_update)
    readRDS(file=filename)
  } else {
    readRDS(file=filename)
  }
}

#' @export
refresh_wbcache <- function(cachedir = rappdirs::user_cache_dir("wbgcharts"), force = FALSE, silent = FALSE) {
  filename <- file.path(cachedir, "wbcache.RData")
  if (!file.exists(filename) |
      force |
      difftime(Sys.time(), file.info(filename)$mtime, units = "days") > REFRESH_PERIOD_DAYS)
  {
    newcache <- wbstats::wbcache()
    if (!file.exists(cachedir)) dir.create(cachedir, recursive = TRUE)
    saveRDS(newcache, file = filename)
  } else {
    if (!silent) {
      message("Countries/series cache is not stale - use force = TRUE to force update")
    }
  }
}

create_wbgref <- function() {
  wb_newcache <- get_wbcache()

  wbgref <- list()

  countries_df <- wb_newcache$countries %>%
    filter(region != "Aggregates")

  wbgref$countries <- list(
    iso2c = countries_df$iso2c,
    iso3c = countries_df$iso3c,
    labels = unlist(modifyList(
      as.list(setNames(trimws(countries_df$country), countries_df$iso3c)),
      list(
        CIV = "Côte d\u2019Ivoire",
        FRO = "Faeroe Islands",
        STP = "São Tomé and Príncipe"
      ))
    ),
    iso2to3 = countries_df %>% select(iso2c, iso3c),
    regions = countries_df %>% select(iso3c, region_iso3c = regionID),
    incomegroups = countries_df %>% select(iso3c, income_iso3c = incomeID)
  )

  regions_df <- wb_newcache$countries %>%
    filter(iso3c %in% c("EAS","ECS","LCN","MEA","NAC","SAS","SSF"))

  wbgref$regions <- list(
    iso2c = regions_df$iso2c,
    iso3c = regions_df$iso3c,
    labels = setNames(trimws(regions_df$country), regions_df$iso3c),
    iso2to3 = regions_df[,c("iso2c", "iso3c")]
  )

  incomes_df <- wb_newcache$countries %>%
    filter(iso3c %in% c("HIC", "UMC", "LMC", "LIC")) %>%
    arrange(match(iso3c, c("HIC", "UMC", "LMC", "LIC")))

  wbgref$incomes <- list(
    iso2c = incomes_df$iso2c,
    iso3c = incomes_df$iso3c,
    labels = setNames(trimws(incomes_df$country), incomes_df$iso3c),
    iso2to3 = incomes_df[,c("iso2c", "iso3c")]
  )

  incomes3_df <- wb_newcache$countries %>%
    filter(iso3c %in% c("HIC", "MIC", "LIC")) %>%
    arrange(match(iso3c, c("HIC", "MIC", "LIC")))

  wbgref$incomes3 <- list(
    iso2c = incomes3_df$iso2c,
    iso3c = incomes3_df$iso3c,
    labels = setNames(trimws(incomes3_df$country), incomes3_df$iso3c),
    iso2to3 = incomes3_df[,c("iso2c", "iso3c")]
  )

  wbgref$all_geo <- list(
    iso2c = wb_newcache$countries$iso2c,
    iso3c = wb_newcache$countries$iso3c,
    labels = setNames(trimws(wb_newcache$countries$country), wb_newcache$countries$iso3c),
    iso2to3 = wb_newcache$countries[,c("iso2c", "iso3c")]
  )

  wbgref <<- wbgref
}

wbgdata_from_databank <- function(country = "all", indicator, startdate, enddate, filename) {
  df <- read.csv(filename, na.strings = "..", stringsAsFactors = FALSE, check.names = FALSE)

  df <- df %>%
    select(iso3c=`Country Code`,indicatorID=`Series Code`, date=`Time`, value=`Value`)

  if (!(all(indicator %in% df$indicatorID)))
    stop("Databank file does not match API call (indicators)")

  if (!(all(c(startdate,enddate) %in% df$date)))
    warning("Databank file does not contain start/end dates")

  #FIXME check & filter countries too

  df %>% filter(indicatorID %in% indicator, date >= startdate, date <= enddate)
}

wbgdata_to_databank <- function(df, filename) {
  df <- df %>%
    select(`Country Code`=iso3c,`Series Code`=indicatorID, `Time`=date, `Value`=value)

  write.csv(df, filename, na = "..", row.names = FALSE)
}

wbgdata_build_cache_filename <- function(country, indicator, startdate, enddate) {
  long <- paste(paste(country, collapse = "+"), paste(indicator, collapse = "+"), startdate, enddate, sep = "_")
  hash <- digest::digest(long, "md5") # not a security context so md5 will be fine
  paste0(hash,".csv")
}

#' Download data from the World Bank API (wrapper for wbstats::wb)
#'
#' \code{wbgdata} is a wrapper for the \code{wbstats::wb} function. It also
#' downloads data using the World Bank API, but offers enhancements including
#' better cached data management, more input parameter formats and more output
#' formats, based boilerplate code I found myself constantly retypting.
#'
#' @param indicator a vector of indicator (SETS) codes. If named and
#'        \code{rename.indicators} is true, the indicator codes will be replaced
#'        by their respective names.
#' @param years an alternative to \code{startdate} and \code{enddate} (which it
#'        overrides), a vector of years for which to download data.
#' @param col.indicator if true, return the indicator (description) column
#' @param cache the wbstats cache object to use (default usually ok)
#' @param indicator.wide return the indicators in wide (not long) format
#' @param offline (experimental)
#' @param rename.indicators see \code{indicator} paramater
#' @inheritParams wbstats::wb
#'
#' @export
wbgdata <-function(country = "all", indicator, startdate, enddate, years, ...,
                   col.indicator = FALSE, cache = get_wbcache(),
                   indicator.wide = TRUE, removeNA = FALSE, offline="none",
                   rename.indicators = FALSE) {
  if (!missing(years)) {
    if (!missing(startdate) | !missing(enddate)) {
      stop("Provide either years or (startdate and enddate) but not both.")
    }
    startdate <- min(years)
    enddate <- max(years)
  }
  offline_path <- ".wbgdata"
  if (offline != "none") {
    offline_file <- file.path(offline_path, wbgdata_build_cache_filename(country, indicator, startdate, enddate))
    if (file.exists(offline_file)) {
      offline_df <- wbgdata_from_databank(country, indicator, startdate, enddate, filename = offline_file)
    } else {
      warning("No offline data found for TODO. Will create.")
      dir.create(offline_path, recursive = TRUE)
      offline <- "overwrite"
    }
  }
  if (offline == "only") {
    df <- offline_df
  } else {
    df <- wbstats::wb(country, indicator, startdate, enddate, removeNA = removeNA, cache = cache,...)

    if (!("iso3c" %in% colnames(df))) {
      # Older versions of wbstats don't return, newer ones do
      df <- df %>% left_join(wbgref$all_geo$iso2to3, by = "iso2c")
    }
    if (!col.indicator)
      df <- df %>% select(-indicator)
    df <- df %>% select(-iso2c, -country)
    df <- df %>% mutate(date = as.numeric(date))

    df <- df %>% select(iso3c, indicatorID, date, value)

    if (offline %in% c("warn", "stop")) {
      same <- all.equal(df %>% arrange(indicatorID, iso3c, date), offline_df %>% arrange(indicatorID, iso3c, date))
      if (same != TRUE) {
        if (offline == "warn") {
          warning(paste0(
            "Data changed, using new data.\n",
            "Rerun with offline='overwrite' to accept new data & eliminate this warning\n",
            "Previous data cached in: ", offline_file
          ))
        } else {
          stop(paste0(
            "Data changed.\n",
            "Rerun with offline='overwrite' to accept new data & eliminate this warning\n",
            "Previous data cached in: ", offline_file
          ))
        }
      }
    } else if (offline == "overwrite") {
      wbgdata_to_databank(df, filename = offline_file)
    } else if (offline == "none") {
      # nothing
    }
  }

  if (!missing(years)) {
    df <- df %>% filter(date %in% years)
  }

  if (!is.null(names(indicator)) && rename.indicators) {
    df <- df %>% mutate(indicatorID = names(indicator)[match(indicatorID, indicator)])
  }

  if (indicator.wide) {
    df <- df %>% tidyr::spread("indicatorID", "value")
  }

  df
}

#' Build a source string for World Bank indicators
#'
#' \code{wbg_source} builds a source string for a set of indicators.
#'
#' For each indicator, it takes the source from the API metadata, then extracts the
#' part leading up to the first period (.), which is usually useable as a short
#' source. These are then combined into a single string
#'
#' Since population-related indicators are commonly included as references, this
#' function excludes them from source string construction.
#'
#' @param indicatorIDs a vector of indicator IDs (SETS codes)
#'
#' @export
#'
wbg_source <- function(indicatorIDs, source = NULL) {
  if (is.null(source)) {
    wdi_ind <- get_wbcache()$indicators

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
  } else {
    sources <- paste0(
      source, ". ",
      "WDI (", paste(indicatorIDs, collapse="; "), ")."
    )
  }
  return(sources)
}

#' @export
endashify <- function(s) {
  gsub("-", "–", s)
}

#' @export
wbg_name_mrv <- function(years) {
  paste0("most recent value in ",min(years, na.rm = TRUE),"–",max(years, na.rm = TRUE))
}

#' @export
wbg_name <- function(indicatorID, indicator, by, year, mrv, denom) {
  if (!missing(indicatorID) && !missing(indicator)) {
    stop("Provide either indicatorID to lookup name, or indicator to custom build - not both.")
  }

  if (!missing(mrv)) {
    year = wbg_name_mrv(mrv)
  }

  if (!missing(indicatorID)) {
    wdi_ind <- get_wbcache()$indicators
    ind_name <- wdi_ind$indicator[wdi_ind$indicatorID == indicatorID]

    m <- regexpr("(?<stem>[^\\(]+)\\((?<paren>[^\\)]+)\\)",ind_name, perl=TRUE)
    if (m == -1) {
      stem <- ind_name
      paren <- NULL
    } else {
      stem <- substr(ind_name, attr(m, "capture.start")[1], attr(m, "capture.start")[1]+attr(m, "capture.length")[1]-1)
      stem <- trimws(stem)
      paren <- substr(ind_name, attr(m, "capture.start")[2], attr(m, "capture.start")[2]+attr(m, "capture.length")[2]-1)

      if (attr(m, "match.length") < nchar(ind_name)) {
        warning(paste0("wbg_name() may be losing indicator name information for ",indicatorID,". Check this."))
      }
    }
  } else {
    stem <- indicator
    paren <- NULL
  }

  if (!missing(denom)) {
    paren <- denom
  }

  return(paste0(
    stem,
    if (!missing(by)) paste0(", ", by),
    if (!missing(year)) paste0(", ", endashify(year)),
    if (!is.null(paren)) paste0(" (", paren, ")")
  ))
}
