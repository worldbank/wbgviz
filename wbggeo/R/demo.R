#' @export
wbg_choropleth <- function(data, maps, style, variable, iso3c = "iso3c", aspect_ratio = 1, fill.values = NULL, na.in.legend = TRUE) {
  if (!na.in.legend) {
    breaks <- unique(data[[variable]])
    breaks <- breaks[!is.na(breaks)]
  } else {
    breaks <- waiver()
  }
  p <- ggplot() +
    geom_map(data = data, aes_string(map_id = iso3c, fill = variable), map = maps$countries) +
    geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") +
    geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white") +
    geom_path(data = maps$boundaries, aes(long, lat, group = group), color = "white", size = 0.2, lineend = maps$boundaries$lineend, linetype = maps$boundaries$linetype) +
    scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
    scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) + {
      if (is.null(fill.values)) {
        scale_fill_manual(palette = style$colors$continuous, na.value = "grey80", breaks = breaks, labels = rename_na("No data"), drop = FALSE)
      } else {
        scale_fill_manual(values = fill.values, na.value = "grey80", breaks = breaks, labels = rename_na("No data"), drop = FALSE)
      }
    } +
    coord_equal() +
    style$theme() +
    style$theme_map(aspect_ratio)


  pg <- wbg_color_disputed(p)
  pg$theme <- style$theme()
  pg
}

#' @export
wbg_bubble_map <- function(data, maps, style, variable, breaks, iso3c = "iso3c", aspect_ratio = 1, max_size = 1, labels=NULL) {
  p <- ggplot(data = data) +
    geom_polygon(data = maps$countries, aes(long, lat, group = group), fill = "grey80") +
    geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") +
    geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white") +
    geom_path(data = maps$boundaries, aes(long, lat, group = group), color = "white", size = 0.2, lineend = maps$boundaries$lineend, linetype = maps$boundaries$linetype) +
    geom_bubble_map(
      aes_string(map_id = iso3c, size = variable),
      centroids = maps$country_centroids,
      fill = style$colors$spot.primary, color = "white", shape = 21, stroke=0.35
    ) +
    scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
    scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
    scale_size_area(max_size = max_size*style$gg_max_point_size, breaks=breaks, labels=labels) +
    coord_equal() +
    style$theme() +
    style$theme_bubble_map()
}

#' @export
wbggeo_demo_atlas_5g_choropleth <- function(year = 2016) {
  indicator <- "SG.GEN.PARL.ZS"
  figure(
    data = function() {
      df <- wbgdata(country = "countries_only", indicator = indicator, startdate = year, enddate = year)
      df$SG.GEN.PARL.ZS <- supercut(df$SG.GEN.PARL.ZS, c(
        "0-15" = "[0,15)",
        "15-30" = "[15,30)",
        "30-45" = "[30,45)",
        "More than 45" = "[45,100]"
      ))
      df
    },
    plot = function(df, style = style_atlas(), quality = "high", aspect_ratio = 1) {
      wbg_choropleth(
        df, wbgmaps[[quality]], style, "SG.GEN.PARL.ZS", aspect_ratio = aspect_ratio
      )
    },
#    theme = style$theme(),
    aspect_ratio = 5/4,
    title = "Women remain underrepresented in national parliaments in most countries.",
    subtitle = paste0(wbg_name(indicator), ", ", year),
    source = paste("Source:", wbg_source(indicator)),
    source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-01-no-poverty.html"
  )
}

#' @export
wbggeo_demo_atlas_6h_bubble <- function(year = 2014) {
  indicators = c("SH.STA.ODFC.ZS","SP.POP.TOTL")
  breaks = c(10e6,100e6,500e6)
  figure(
    data = function() {
      df <- wbgdata(country = "countries_only", indicator = indicators, startdate = year, enddate = year)
      df$count <- df$SH.STA.ODFC.ZS / 100 * df$SP.POP.TOTL
      df
    },
    plot = function(df, style = style_atlas(), quality = "high", aspect_ratio = 1) {
      wbg_bubble_map(
        df, wbgmaps[[quality]], style, "count", breaks, aspect_ratio = aspect_ratio, max_size = 0.7, labels = millions()
      )
    },
#    theme = style_atlas()$theme(),
    aspect_ratio = 5/4,
    title = "Open defecation is widespread throughout parts of South Asia and Sub-Saharan Africa",
    subtitle = paste0("Number of people practicing open defecation, ", year),
    source = paste("Source:", wbg_source(indicators)),
    source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-05-clean-water.html"
  )
}

