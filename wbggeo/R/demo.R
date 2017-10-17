library(ggplot2)
library(wbgcharts)
library(wbggeo)
library(extrafont)

style <- wbgcharts::style_atlas()
quality = "high"
aspect_ratio = 1
cache <- wbstats::wbcache()

wbg_choropleth <- function(data, maps, style, variable, iso3c = "iso3c", aspect_ratio = 1) {
  p <- ggplot() +
    geom_map(data = data, aes_string(map_id = iso3c, fill = variable), map = maps$countries) +
    geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") +
    geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white") +
    geom_path(data = maps$boundaries, aes(long, lat, group = group), color = "white", size = 0.2, lineend = maps$boundaries$lineend, linetype = maps$boundaries$linetype) +
    scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
    scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
    scale_fill_manual(palette = style$colors$continuous, na.value = "grey80", labels = rename_na("No data")) +
    coord_equal() +
    style$theme() +
    style$theme_map(aspect_ratio)

  pg <- wbg_color_disputed(p)
  pg
}

wbg_bubble_map <- function(data, maps, style, variable, breaks, iso3c = "iso3c", aspect_ratio = 1, max_size = 1) {
  p <- ggplot(data = data) +
    geom_polygon(data = maps$countries, aes(long, lat, group = group), fill = "grey80") +
    geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") +
    geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white") +
    geom_path(data = maps$boundaries, aes(long, lat, group = group), color = "white", size = 0.2, lineend = maps$boundaries$lineend, linetype = maps$boundaries$linetype) +
    geom_bubble_map(
      aes_string(map_id = iso3c, size = variable),
      centroids = maps$country_centroids,
      fill = style$colors$spot.primary, color = "white", shape = 21, stroke=0.5
    ) +
    scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
    scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
    scale_size_area(max_size = max_size*style$gg_max_point_size, breaks=breaks, labels=millions()) +
    coord_equal() +
    style$theme() +
    style$theme_bubble_map()
}

wbggeo_demo_atlas_5g_choropleth <- function(style = style_atlas(), quality = "high", aspect_ratio = 1) {
  indicator <- "SG.GEN.PARL.ZS"
  df <- wbgdata(country = "countries_only", indicator = indicator, startdate = 2016, enddate = 2016, cache = cache)

  df$SG.GEN.PARL.ZS <- supercut(
    df$SG.GEN.PARL.ZS,
    c("[0,15)", "[15,30)", "[30,45)", "[45,100]"),
    c("0-15", "15-30", "30-45", "More than 45")
  )

  maps <- wbgmaps[[quality]]

  pg <- wbg_choropleth(
    df, wbgmaps[[quality]], style, "SG.GEN.PARL.ZS", aspect_ratio = aspect_ratio
  )

  figure(
    pg,
    theme = style$theme(),
    aspect_ratio = 5/4,
    title = "Countries in Sub-Saharan Africa have low rates of electrification.",
    subtitle = wbg_name(indicator),
    source = paste("Source:", wbg_source(indicator)),
    source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-01-no-poverty.html"
  )
}

wbggeo_demo_bubble <- function(style = style_atlas(), quality = "high", aspect_ratio = 1) {
  indicator <- "SP.POP.TOTL"
  df <- wbgdata(country = "countries_only", indicator = indicator, startdate = 2014, enddate = 2014, cache = cache)

  maps <- wbgmaps[[quality]]
  breaks <- c(1e7,1e8,5e8,1e9)

  p <- wbg_bubble_map(
    df, wbgmaps[[quality]], style, "SP.POP.TOTL", breaks, aspect_ratio = aspect_ratio, max_size = 0.6
  )

  figure(
    p,
    theme = style$theme(),
    aspect_ratio = 5/4,
    title = "China and India have pretty big populations.",
    subtitle = paste(wbg_name(indicator), "(millions)"),
    source = paste("Source:", wbg_source(indicator)),
    source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-01-no-poverty.html"
  )
}
