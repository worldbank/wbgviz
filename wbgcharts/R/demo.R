#' @export
#' @import ggplot2
wbgcharts_demo_region_bars <- function(style = style_atlas()) {
  indicator <- "SP.POP.TOTL"
  df <- wbgdata(country = wbgref$regions$iso3c, indicator = indicator, startdate = 2015, enddate = 2015)
  df <- df %>% arrange(value) %>% mutate(iso3c = factor(iso3c, iso3c))
  p <- ggplot(df, aes(x = iso3c, y = value, fill=iso3c)) +
    geom_col() +
    geom_bartext(aes(label = billions(1)(value)), family = calc_element("axis.text.y", style$theme())$family, size = style$gg_text_size ) +
    coord_flip() +
    scale_fill_manual(values = style$colors$regions, labels = style$labels$regions) +
    scale_x_discrete(labels = style$labels$regions) +
    scale_y_continuous(labels = billions(1)) +
    style$theme_barchart() +
    theme(legend.position = "none")
  figure(
    p,
    title = "East Asia & Pacific is definitely the most populous region.",
    subtitle = paste(wbg_name(indicator), "(billions)"),
    source = paste("Source:", wbg_source(indicator)),
    source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-01-no-poverty.html"
  )
}

#' @export
wbgcharts_demo_region_cols <- function(style = style_atlas()) {
  df <- wbgdata(country = wbgref$regions$iso3c, indicator = "NY.GDP.MKTP.CD", startdate = 2015, enddate = 2015)
  p <- ggplot(df, aes(x = iso3c, y = value, fill=iso3c)) +
    geom_col() +
    scale_fill_manual(values = style$colors$regions, labels = style$labels$regions) +
    scale_x_discrete(labels = style$labels$regions_wrapped) +
    scale_y_continuous(labels = trillions()) +
    style$theme() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(hjust = 1, angle = 60)
    )
  figure(
    p,
    theme =style$theme(),
    title = "East Asia & Pacific is winning, but Europe & Central Asia is surprisingly close behind.",
    subtitle = "GDP (current US$, trillions)",
    note = "Note: Consider a column chart instead so the axis labels fit more easily",
    source = "Source: This script. WDI (ZX.ABC.DEFG).",
    source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-01-no-poverty.html"
  )
}

#' @import wbstats tidyr
#' @export
wbgcharts_demo_atlas_2a_stackedarea <- function(style = style_atlas()) {
  # Basic chart
  p <- ggplot(data = subset(stunting, iso3c != "NAC"), aes(x = date, y = stunting, fill = iso3c)) +
    geom_lined_area(position="stack") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(breaks = c(0,100,200,300)*1e6, limits=c(0,300)*1e6, labels = c(0,100,200,300)) +
    scale_fill_manual(values = style$colors$regions, labels = style$labels$regions) +
    style$theme() +
    theme(legend.justification = c(0, 1))#, legend.position = "none")

  figure(
    p,
    title = "Child stunting is steadily declining in most regions but increasing in Sub-Saharan Africa",
    subtitle = "Number of children under age 5 that are stunted, height for age (millions)",
    source = "Source: United Nations Children's Fund, World Health Organization and World Bank, 2016, Levels and Trends in Child Malnutrition, New York; WDI (SH.STA.STNT.ZS).",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg2#stunting"
  )
}

#' @import dplyr
#' @export
wbgcharts_demo_atlas_1a_panel <- function(style = style_atlas()) {

  df <- data.frame(
    region = factor(names(style$colors$regions), levels = rev(names(style$colors$regions)), ordered = T),
    year = c(rep(1990, 7), rep(2013, 7)),
    poverty = c(966, 9, 71, 14, 0, 505, 276,
                71, 10, 34, NA, 0, 256, 389),
    percent = c(60, 2, 16, 6, 0, 45, 54,
                4, 2, 5, NA, 0, 15, 42)
  )
  df  <- df[!(df$region %in% c("NAC")),]

  p_absolute <- ggplot(data = df, aes(y = poverty, x = region)) +
    geom_col(fill = style$colors$spot.primary, width=0.7) +
    geom_bartext(family=calc_element("axis.text.y", style$theme())$family, size=style$gg_text_size, color.outside = style$colors$spot.primary) +
    coord_flip() +
    facet_grid(. ~ year) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_discrete(labels = style$labels$regions) +
    labs(title = "Millions") +
    style$theme_barchart() +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(size=rel(1), face="bold", family="Avenir Heavy", color="#575756",hjust=0),
      plot.title = element_text(size=rel(1), face="plain", family="Avenir Book")
    )

  p_percent <- ggplot(data = df, aes(y = percent, x = region)) +
    geom_percent_col(width=0.7, fill = style$colors$spot.secondary, fill.bg = style$colors$spot.secondary.light) +
    geom_bartext(family=calc_element("axis.text.y", style$theme())$family, size=style$gg_text_size, color.outside = style$colors$spot.secondary) +
    coord_flip() +
    facet_grid(. ~ year) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 100)) +
    scale_x_discrete(labels = style$labels$regions) +
    labs(title = "Percent") +
    style$theme_barchart() +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(size=rel(1), face="bold", family="Avenir Heavy", color="#575756",hjust=0),
      plot.title = element_text(size=rel(1), face="plain", family="Avenir Book")
    )

  p <- interleave_panels_horizontal(p_absolute, p_percent, width.ratio = c(7, 2))
  figure(
    p,
    theme = style$theme(),
    title = "The extreme poverty rate and the number of people living in extreme poverty have fallen in almost every region",
    subtitle = "Number and share of population living on less than $1.90 a day (2011 purchasing power parity or PPP) (%), 1990 and 2013",
    note = "Note: For this indicator, regional aggregates exclude certain high income countries (World Bank Group, Poverty and Shared Prosperity 2016: Taking on Inequality. Washington, DC: World Bank., p. 49). 2013 estimates for Middle East and North Africa are not shown because survey coverage is too low.",
    source = "Source: World Bank, Development Research Group. WDI (SI.POV.DDAY, SP.POP.TOTL).",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg1"
  )
}

make_data_2b <- function() {
  countries <- c("TLS","BDI", "MWI", "YEM", "PAK", "BEN", "ETH", "SLE", "LAO", "RWA")
  df <- wb(country = countries, indicator = c("SH.STA.STNT.Q1.ZS", "SH.STA.STNT.Q5.ZS", "SH.STA.STNT.ZS"), startdate = 2010, enddate = 2015)
  df <- df %>% left_join(wbstats::wb_cachelist$countries[,c("iso2c", "iso3c")])
  df <- df %>% select(date, iso3c, indicatorID, indicator, value)

  stunting_quintiles <- df %>% group_by(iso3c, indicatorID, indicator) %>% filter(date == max(date)) %>% ungroup()
  save(stunting_quintiles, file="./data/stunting_quintiles.rda")
}

#' @import dplyr
#' @export
wbgcharts_demo_atlas_2b_dot <- function(style = style_atlas()) {
  indicators <- list(
    `SH.STA.STNT.Q1.ZS` = "Poorest quintile",
    `SH.STA.STNT.ZS` = "Average",
    `SH.STA.STNT.Q5.ZS` = "Richest quintile"
  )
  iso3c_ordered <- stunting_quintiles %>%
    filter(indicatorID == "SH.STA.STNT.ZS") %>%
    arrange(-value) %>%
    pull(iso3c)
  stunting_quintiles <- stunting_quintiles %>%
    mutate(iso3c = factor(iso3c, levels = iso3c_ordered)) %>%
    mutate(indicatorID = factor(indicatorID, levels = names(indicators)))

  p <- ggplot(stunting_quintiles, aes(x = iso3c, y = value, color = indicatorID, shape = indicatorID)) +
    geom_point(size = 1.25, stroke = 2) +
    scale_color_manual(values=style$colors$categorical, labels = indicators) +
    scale_shape_manual(values=style$shapes$categorical, labels = indicators) +
    scale_y_continuous(limits = c(0, 80)) +
    scale_x_discrete(labels = wbgref$countries$labels) +
    style$theme() +
    theme(legend.position = "top", legend.justification = "left")

  figure(
    p,
    title = "Child stunting can vary as much within countries as between countries",
    subtitle = "Share of children under 5 that are stunted, height for age (%)",
    source = "Source: World Bank Health, Nutrition and Population Statistics database (SH.STA.STNT.Q1.ZS, SH.STA.STNT.Q5.ZS, SH.STA.STNT.QT.ZS).",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg2#stunting"
  )
}

make_data_6f <- function() {
  df <- wb(country = wbgref$countries$iso3c,
           indicator = c("SH.H2O.SAFE.ZS", "SH.H2O.SAFE.RU.ZS"),
           startdate = 2012, enddate = 2012)
  df <- df %>% left_join(wbgref$countries$iso2to3)
  df <- df %>% select(iso3c, indicatorID, indicator, value)

  water_inequality <- df
  save(water_inequality, file="./data/water_inequality.rda")
}

#' @import dplyr
#' @import tidyr
#' @export
wbgcharts_demo_atlas_6f_dot <- function(style = style_atlas()) {
  indicator_labels <- list(
    `SH.H2O.SAFE.RU.ZS` = "Rural",
    `SH.H2O.SAFE.ZS` = "Average"
  )

  bottom30 <- water_inequality %>%
    filter(indicatorID == "SH.H2O.SAFE.RU.ZS") %>%
    arrange(-value) %>%
    tail(30) %>%
    pull(iso3c)

  df <- water_inequality %>%
    filter(iso3c %in% bottom30) %>%
    mutate(iso3c = factor(iso3c, levels = bottom30))

  p <- ggplot(data = df, aes(x = value, y = iso3c, color = indicatorID, shape = indicatorID)) +
    geom_other_dotplot(aes(y = iso3c), size.line=0.25) +
    geom_other_dotplot_label(
      aes(x = value, y = iso3c, label = wbgref$countries$labels[iso3c]),
      side = "left", nudge_x = -1,
      size = style$gg_text_size * 0.8, color = style$theme()$text$colour,
      family = style$theme()$text$family
    ) +
    scale_x_continuous(limits = c(20, 80)) +
    scale_color_manual(values=style$colors$categorical, labels = indicator_labels) +
    scale_shape_manual(values=style$shapes$categorical, labels = indicator_labels) +
    style$theme() +
    style$theme_barchart() +
    theme(axis.text.y = element_blank(),
          legend.position = c(0.9,1), legend.justification = c(1,1),
          legend.direction = "horizontal", legend.margin = margin())

  figure(
    p,
    aspect_ratio = 3/4,
    title = "The [poorest] people in rural areas suffer from especially low access to water...",
    subtitle = "Share of ppulation with access to an improved water source, national average and rural, 2012 (%)",
    source = "Source: World Health Organization/United Nations Children's Fund Joint Monitoring Programme for Water Supply and Sanitation; WDI (SH.H2O.SAFE.ZS, SH.H2O.SAFE.UR.ZS)",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg6#inequality"
  )
}

make_data_8c <- function() {
  df <- wb(country = wbgref$countries$iso3c,
           indicator = c("SL.UEM.NEET.FE.ZS", "SL.UEM.NEET.MA.ZS"),
           startdate = 2010, enddate = 2014)
  df <- df %>% left_join(wbgref$countries$iso2to3)
  df <- df %>% select(iso3c, date, indicatorID, value)

  df_wide <- df %>%
    spread(key = indicatorID, value = value) %>%
    filter(complete.cases(.))

  df_wide <- df_wide %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()

  neet_by_gender <- df_wide
  save(neet_by_gender, file="./data/neet_by_gender.rda")
}

#' @import dplyr
#' @import tidyr
#' @export
wbgcharts_demo_atlas_8c_scatter <- function(style = style_atlas()) {
  p <- ggplot(neet_by_gender, aes(x = SL.UEM.NEET.MA.ZS, y = SL.UEM.NEET.FE.ZS)) +
    geom_point(fill = style$colors$spot.primary, color = "white", shape = 21, size = 2, stroke=0.25) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", size=0.25) +
    scale_x_continuous(expand = c(0,0), limits = c(0, 80)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
    xlab("Share of male youth population (ages 15-24) not in employment, education or training,\nmost recent year available during 2010-14 (%)") +
    style$theme() +
    theme(panel.grid.major.x = style$theme()$panel.grid.major.y,
          axis.title = element_text(hjust=0.5, lineheight = 1, size = rel(0.9)),
          axis.title.y = element_blank())

  figure(
    p,
    aspect_ratio = 1/1.1,
    title = "Young women are more likely than young men to be economically inactive and not in school",
    subtitle = "Share of female youth population (ages 15-24) not in employment, education or training, most recent year available during 2010-14 (%)",
    source = "Source: International Labour Organization Key Indicators of the Labour Market database; WDI (SL.UEM.NEET.FE.ZS, SL.UEM.NEET.MA.ZS)",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg8#neet"
  )
}

make_data_4k <- function() {
  df <- wb(country = "all",
           indicator = c("SE.PRM.UNER"),
           startdate = 1990, enddate = 2014)
  df <- df %>% left_join(wbgref$all_geo$iso2to3)
  df <- df %>% select(iso3c, date, indicatorID, value)
  df <- df %>% mutate(date = as.numeric(date))

  out_of_school <- df

  save(out_of_school, file="./data/out_of_school.rda")
}

#' @import dplyr
#' @import tidyr
#' @import gtable
#' @export
wbgcharts_demo_atlas_4k_area_panels <- function(style = style_atlas()) {
  # impute EAS 1997
  df <- out_of_school %>% filter(iso3c %in% wbgref$regions$iso3c) %>% select(-indicatorID)
  df <- rbind(df, data.frame(
    iso3c = "EAS",
    date = 1997,
    value = mean(
      df$value[df$iso3c == "EAS" & df$date == 1996],
      df$value[df$iso3c == "EAS" & df$date == 1998]
    )
  ))

  # Tidy up country data set
  df_countries <- out_of_school %>%
    filter(iso3c %in% wbgref$countries$iso3c) %>%
    select(-indicatorID) %>%
    left_join(wbgref$countries$regions)

  # Get MRV for each country in >= 2010
  df_countries <- df_countries %>%
    group_by(iso3c, region_iso3c) %>%
    filter(date == max(date) & date >= 2010) %>%
    ungroup

  # And we'll only break out relatively large countries
  df_countries <- df_countries %>%
    arrange(-value) %>%
    filter(value > 2.5e6) %>%
    mutate(country = TRUE)

  df_countries_totals <- df_countries %>%
    select(-date) %>%
    group_by(region_iso3c) %>%
    summarise(breakout_sum = sum(value))

  # Now some dplyr magic to create a combined dataset
  df_mrv_breakout <- df %>%
    filter(date == 2014) %>%
    mutate(region_iso3c = iso3c, country=FALSE) %>%
    left_join(df_countries_totals) %>%
    mutate(breakout_sum = ifelse(is.na(breakout_sum), 0, breakout_sum)) %>%
    mutate(value = value - breakout_sum) %>%
    mutate(iso3c = ifelse(breakout_sum > 0, paste0(iso3c, ".rest"), iso3c)) %>%
    select(-breakout_sum) %>%
    bind_rows(df_countries)

  # Finally we need to sort in display order, that is by countries by value, then rest
  iso3c_sorted <- df_mrv_breakout %>%
    arrange(region_iso3c, -grepl(".rest", iso3c), value) %>%
    pull(iso3c)

  df_mrv_breakout <- df_mrv_breakout %>%
    mutate(iso3c = factor(iso3c, levels = iso3c_sorted)) %>%
    arrange(iso3c) %>%
    mutate(
      cum_value = rev(cumsum(rev(value))),
      label_y = (cum_value + lead(cum_value, default = 0))/2
    )

  p <- ggplot(df, aes(x = date, y = value, fill = iso3c)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 125)*1e6, breaks=(0:5)*25e6, labels = millions()) +
    style$theme() +
    theme(
      legend.position = "none",
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      plot.margin = margin(1, 0, 1, 1, unit = "lines")
    )

  p.bar <- ggplot(df_mrv_breakout, aes(
      x = "2010-14",
      y = value,
      fill=region_iso3c,
      group=iso3c
    )) +
    geom_col(position = "stack") +
    geom_hline(
      data = df_mrv_breakout %>% filter(country),
      mapping = aes(yintercept = cum_value),
      color="white", size = 0.35) +
    geom_text(
      data = df_mrv_breakout %>% filter(country),
      mapping = aes(x = 1, y = label_y,label = wbgref$countries$labels[as.character(iso3c)]),
      hjust = 0, nudge_x = 0.55,
      size = style$gg_text_size*0.8,
      color = style$theme()$text$colour,
      family = style$theme()$text$family) +
    scale_fill_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 125)*1e6, breaks=(0:5)*25e6, labels = millions()) +
    style$theme() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(1, 2, 1, 0.5, unit = "lines")
    )

  pt.bar <- ggplotGrob(p.bar)
  pt.bar$layout$clip[pt.bar$layout$name=="panel"] <- "off"

  chart <- gtable_row("chart", list(ggplotGrob(p), pt.bar), height = unit(1, "null"), widths = unit(c(6,1), "null"))

  figure(
    chart,
    aspect_ratio = 3/2,
    theme = style$theme(),
    title = "The number of children out of school fell across regions",
    subtitle = "Primary school-age children out of school, by region (millions)",
    source = "Source: United Nations Educational Scientific and Cultural Organization Institute for Statistics; WDI (SE.PRM.UNER).",
    note = "Note: 1997 East Asia & Pacific imputed.",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg4#outofschool"
  )
}

make_data_17c <- function() {
  df <- wb(country = "LIC", indicator = "BX.KLT.DINV.WD.GD.ZS", startdate = 2000, enddate = 2015)
  df <- df %>% left_join(wbstats::wb_cachelist$countries[,c("iso2c", "iso3c")])
  df <- df %>% select(date, iso3c, indicatorID, indicator, value)
  df <- df %>% mutate(date = as.numeric(date))

  fdi_to_lics <- df
  save(fdi_to_lics, file="./data/fdi_to_lics.rda")
}

#' @import dplyr
#' @export
wbgcharts_demo_atlas_17c_line <- function(style = style_atlas()) {
  p <- ggplot(fdi_to_lics, aes(x = date, y = value)) +
    geom_line(color = style$colors$spot.primary, size = 1) +
    scale_x_continuous(expand = c(0,0)) +
    style$theme()

  figure(
    p,
    aspect_ratio = 1/2,
    title = "FDI flows to low-income countries have been falling since 2011 and stood at around 4 percent of GDP in 2015...",
    subtitle = "FDI, net inflows to low-income countries (% of GDP)",
    source = "Source: IMF, International Financial Statistics and Balance of Payments databases; World Bank, International Debt Statistics; World Bank and OECD GDP extimates; WDI (BX.KLT.DINV.WD.GD.ZS).",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg17#fdi"
  )
}

#' @export
wbgcharts_demo_atlas_all <- function(path, style = style_atlas) {
  figure_save_allformats(wbgcharts_demo_atlas_1a_panel, paste0(path, "1a"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_2a_stackedarea, paste0(path, "2a"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_2b_dot, paste0(path, "2b"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_4k_area_panels, paste0(path, "4k"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_6f_dot, paste0(path, "6f"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_8c_scatter, paste0(path, "8c"), style = style)
}
