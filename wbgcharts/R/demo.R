#' @export
wbgcharts_demo_region_bars <- function() {
  indicator = "SP.POP.TOTL"
  figure(
    data = function() {
      df <- wbgdata(country = wbgref$regions$iso3c, indicator = indicator, startdate = 2015, enddate = 2015, indicator.wide = F)
      df <- df %>% arrange(value) %>% mutate(iso3c = factor(iso3c, iso3c))
      df
    },
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x = iso3c, y = value, fill=iso3c)) +
        geom_col() +
        geom_bartext(aes(label = billions(1)(value)), family = calc_element("axis.text.y", style$theme())$family, size = style$gg_text_size ) +
        coord_flip() +
        scale_fill_manual(values = style$colors$regions, labels = style$labels$regions) +
        scale_x_discrete(labels = style$labels$regions) +
        scale_y_continuous(labels = billions(1)) +
        style$theme() +
        style$theme_barchart() +
        theme(legend.position = "none")
    },
    title = "East Asia & Pacific is definitely the most populous region.",
    subtitle = paste(wbg_name(indicator), "(billions)"),
    source = paste("Source:", wbg_source(indicator)),
    source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-01-no-poverty.html"
  )
}

#' @export
wbgcharts_demo_region_cols <- function() {figure(
  data = function() {
    df <- wbgdata(country = wbgref$regions$iso3c, indicator = "NY.GDP.MKTP.CD", startdate = 2015, enddate = 2015, indicator.wide = F)
    df
  },
  plot = function(df, style = style_atlas()) {
    ggplot(df, aes(x = iso3c, y = value, fill=iso3c)) +
      geom_col() +
      scale_fill_manual(values = style$colors$regions, labels = style$labels$regions) +
      scale_x_discrete(labels = style$labels$regions_wrapped) +
      scale_y_continuous(labels = trillions()) +
      style$theme() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(hjust = 1, angle = 60)
      )
  },
  title = "East Asia & Pacific is winning, but Europe & Central Asia is surprisingly close behind.",
  subtitle = "GDP (current US$, trillions)",
  note = "Note: Consider a column chart instead so the axis labels fit more easily",
  source = "Source: This script. WDI (ZX.ABC.DEFG).",
  source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-01-no-poverty.html"
)}

#' @export
wbgcharts_demo_bullets <- function(N = 10) {figure(
  data <- function() {
    df <- wbgdata(
      country = wbgref$countries$iso3c,
      indicator = c("SE.SEC.NENR.MA", "SE.SEC.NENR.FE"),
      startdate = 2010, enddate = 2015
    )

    # Get the most recent year for each country
    df <- df %>%
      filter(complete.cases(.)) %>%
      group_by(iso3c) %>%
      filter(date == max(date)) %>%
      ungroup()

    # Find the top N countries by gap, but order by FE
    bottom <- df %>%
      arrange(SE.SEC.NENR.MA - SE.SEC.NENR.FE) %>%
      tail(N) %>%
      arrange(-SE.SEC.NENR.FE) %>%
      pull(iso3c)

    # Reorder & reshape for ggplotting
    df.long <- df %>%
      filter(iso3c %in% bottom) %>%
      mutate(iso3c = factor(iso3c, levels = bottom)) %>%
      tidyr::gather(indicatorID, value, SE.SEC.NENR.FE, SE.SEC.NENR.MA)

    df.long
  },
  plot = function(df.long, style = style_atlas()) {
    p <- ggplot(df.long, aes(x = iso3c, y = value, fill=indicatorID)) +
      geom_col(position = "bullet") +
      scale_fill_manual(values = style$colors$categorical, labels = c("Female", "Male")) +
      scale_x_discrete(labels = wbgref$countries$labels) +
      scale_y_continuous(labels = round, expand = c(0, 0), limits = c(0, 100)) +
      coord_flip() +
      style$theme() +
      style$theme_barchart() +
      theme(legend.position = c(1,1), legend.justification = c(1,1), legend.direction = "horizontal")
  },
  title = "The countries with the largest enrolment gender gaps are mostly low income, with one surprising exception.",
  subtitle = "Net enrolment rate, secondary (%)",
  note = paste("Note:", N, "countries with largest gap between male and female enrolment, ordered by female enrolment (low to high)"),
  source = paste("Source:",wbg_source("SE.SEC.NENR")),
  source_url = "http://datatopics.worldbank.org/sdgatlas/SDG-04-quality-education.html"
)}

#' @export
wbgcharts_demo_atlas_2a_stackedarea <- function() {figure(
  data = function() {
    subset(stunting, iso3c != "NAC")
  },
  plot = function(df, style = style_atlas()) {
    ggplot(data = df, aes(x = date, y = stunting, fill = iso3c)) +
      geom_lined_area(position="stack") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(limits=c(0,300)*1e6, labels = millions()) +
      scale_fill_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
      style$theme() +
      theme(legend.justification = c(0, 1))#, legend.position = "none")
  },
  title = "Child stunting is steadily declining in most regions but increasing in Sub-Saharan Africa",
  subtitle = "Number of children under age 5 that are stunted, height for age (millions)",
  source = "Source: United Nations Children's Fund, World Health Organization and World Bank, 2016, Levels and Trends in Child Malnutrition, New York; WDI (SH.STA.STNT.ZS).",
  source_url = "http://datatopics.worldbank.org/sdgatlas/sdg2#stunting"
)}

#' @export
wbgcharts_demo_atlas_1a_panel <- function() {figure(
  data = function() {
    df <- data.frame(
      region = factor(wbgref$regions$iso3c, levels = wbgref$regions$iso3c, ordered = T),
      year = c(rep(1990, 7), rep(2013, 7)),
      poverty = c(966, 9, 71, 14, 0, 505, 276,
                  71, 10, 34, NA, 0, 256, 389),
      percent = c(60, 2, 16, 6, 0, 45, 54,
                  4, 2, 5, NA, 0, 15, 42)
    )
    df  <- df[!(df$region %in% c("NAC")),]
    df
  },
  plot <- function(df, style = style_atlas()) {
    p_absolute <- ggplot(data = df, aes(y = poverty, x = region)) +
      geom_col(fill = style$colors$spot.primary, width=0.7) +
      geom_bartext(family=calc_element("axis.text.y", style$theme())$family, size=style$gg_text_size, color.outside = style$colors$spot.primary) +
      coord_flip() +
      facet_grid(. ~ year) +
      scale_y_continuous(expand=c(0,0)) +
      scale_x_discrete(labels = style$labels$regions) +
      labs(title = "Millions") +
      style$theme() +
      style$theme_barchart() +
      theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = rel(1.0), face="bold",hjust=0),
        plot.title = element_text(size = rel(1.0), face="plain")
      )

    p_percent <- ggplot(data = df, aes(y = percent, x = region)) +
      geom_percent_col(width=0.7, fill = style$colors$spot.secondary, fill.bg = style$colors$spot.secondary.light) +
      geom_bartext(family=calc_element("axis.text.y", style$theme())$family, size=style$gg_text_size, color.outside = style$colors$spot.secondary) +
      coord_flip() +
      facet_grid(. ~ year) +
      scale_y_continuous(expand=c(0,0), limits=c(0, 100)) +
      scale_x_discrete(labels = style$labels$regions) +
      labs(title = "Percent") +
      style$theme() +
      style$theme_barchart() +
      theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = rel(1.0), face="bold",hjust=0),
        plot.title = element_text(size = rel(1.0), face="plain")
      )

    g <- interleave_panels_horizontal(p_absolute, p_percent, width.ratio = c(7, 2))
    g$theme <- style$theme()
    g
  },
#  theme = style_atlas()$theme(),
  title = "The extreme poverty rate and the number of people living in extreme poverty have fallen in almost every region",
  subtitle = "Number and share of population living on less than $1.90 a day (2011 purchasing power parity or PPP) (%), 1990 and 2013",
  note = "Note: For this indicator, regional aggregates exclude certain high income countries (World Bank Group, Poverty and Shared Prosperity 2016: Taking on Inequality. Washington, DC: World Bank., p. 49). 2013 estimates for Middle East and North Africa are not shown because survey coverage is too low.",
  source = "Source: World Bank, Development Research Group. WDI (SI.POV.DDAY, SP.POP.TOTL).",
  source_url = "http://datatopics.worldbank.org/sdgatlas/sdg1"
)}

#' @export
wbgcharts_demo_atlas_2b_dot <- function() {
  countries <- c("TLS","BDI", "MWI", "YEM", "PAK", "BEN", "ETH", "SLE", "LAO", "RWA")
  indicators <- c(
    "Poorest quintile" = "SH.STA.STNT.Q1.ZS",
    "Average" = "SH.STA.STNT.ZS",
    "Richest quintile" = "SH.STA.STNT.Q5.ZS"
  )
  figure(
    data = function() {
      # Get data
      df <- wbgdata(country = countries, indicator = indicators,
                    startdate = 2010, enddate = 2015,
                    indicator.wide = FALSE, removeNA = TRUE)

      # Latest available per country
      df <- df %>% group_by(iso3c, indicatorID) %>% filter(date == max(date)) %>% ungroup()

      df
    },
    plot = function(df, style = style_atlas()) {
      iso3c_ordered <- df %>%
        filter(indicatorID == "SH.STA.STNT.ZS") %>%
        arrange(-value) %>%
        pull(iso3c)
      df <- df %>%
        mutate(iso3c = factor(iso3c, levels = iso3c_ordered)) %>%
        mutate(indicatorID = factor(indicatorID, levels = indicators))

      labels <- setNames(names(indicators), indicators) # Flip list for labelling
      p <- ggplot(df, aes(x = iso3c, y = value, color = indicatorID, shape = indicatorID)) +
        geom_point(size = 1.25, stroke = 2) +
        scale_color_manual(values=style$colors$categorical, labels = labels) +
        scale_shape_manual(values=style$shapes$categorical, labels = labels) +
        scale_y_continuous(limits = c(0, 80)) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        style$theme() +
        theme(legend.position = "top", legend.justification = "left")
    },
    title = "Child stunting can vary as much within countries as between countries",
    subtitle = "Share of children under 5 that are stunted, height for age (%)",
    source = "Source: World Bank Health, Nutrition and Population Statistics database (SH.STA.STNT.Q1.ZS, SH.STA.STNT.Q5.ZS, SH.STA.STNT.QT.ZS).",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg2#stunting"
  )
}

#' @export
wbgcharts_demo_atlas_6f_dot <- function() {
  indicators <- c(
    "Rural" = "SH.H2O.SAFE.RU.ZS",
    "Average" = "SH.H2O.SAFE.ZS"
  )
  figure(
    data = function() {
      df <- wbgdata(country = wbgref$countries$iso3c, indicator = indicators,
                    startdate = 2012, enddate = 2012,
                    removeNA = TRUE, indicator.wide = FALSE)

      bottom30 <- df %>%
        filter(indicatorID == "SH.H2O.SAFE.RU.ZS") %>%
        arrange(-value) %>%
        tail(30) %>%
        pull(iso3c)

      df <- df %>%
        filter(iso3c %in% bottom30) %>%
        mutate(iso3c = factor(iso3c, levels = bottom30))
      df
    },
    plot = function(df, style = style_atlas()) {
      labels <- setNames(names(indicators), indicators) # Flip list for labelling
      p <- ggplot(data = df, aes(x = value, y = iso3c, color = indicatorID, shape = indicatorID)) +
        geom_other_dotplot(aes(y = iso3c), size.line=0.25) +
        geom_other_dotplot_label(
          aes(x = value, y = iso3c, label = wbgref$countries$labels[iso3c]),
          side = "left", nudge_x = -1,
          size = style$gg_text_size * 0.8, color = style$theme()$text$colour,
          family = style$theme()$text$family
        ) +
        scale_x_continuous(limits = c(20, 80)) +
        scale_color_manual(values=style$colors$categorical, labels = labels) +
        scale_shape_manual(values=style$shapes$categorical, labels = labels) +
        style$theme() +
        style$theme_barchart() +
        theme(axis.text.y = element_blank(),
              legend.position = c(0.9,1), legend.justification = c(1,1),
              legend.direction = "horizontal", legend.margin = margin())
    },
    aspect_ratio = 3/4,
    title = "The [poorest] people in rural areas suffer from especially low access to water...",
    subtitle = "Share of population with access to an improved water source, national average and rural, 2012 (%)",
    source = "Source: World Health Organization/United Nations Children's Fund Joint Monitoring Programme for Water Supply and Sanitation; WDI (SH.H2O.SAFE.ZS, SH.H2O.SAFE.UR.ZS)",
    source_url = "http://datatopics.worldbank.org/sdgatlas/sdg6#inequality"
  )
}

#' @export
wbgcharts_demo_atlas_8c_scatter <- function() {figure(
  data = function() {
    df <- wbgdata(country = wbgref$countries$iso3c,
             indicator = c("SL.UEM.NEET.FE.ZS", "SL.UEM.NEET.MA.ZS"),
             startdate = 2010, enddate = 2014)

    df <- df %>%
      filter(complete.cases(.)) %>%
      group_by(iso3c) %>%
      filter(date == max(date)) %>%
      ungroup()

    df
  },
  plot = function(df, style = style_atlas()) {
    ggplot(df, aes(x = SL.UEM.NEET.MA.ZS, y = SL.UEM.NEET.FE.ZS)) +
      geom_point(fill = style$colors$spot.primary, color = "white", shape = 21, size = 0.1*style$gg_max_point_size, stroke=0.25) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", size=0.25) +
      scale_x_continuous(expand = c(0,0), limits = c(0, 80)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
      xlab("Share of male youth population (ages 15-24) not in employment, education or training,\nmost recent year available during 2010-14 (%)") +
      style$theme() +
      theme(panel.grid.major.x = style$theme()$panel.grid.major.y,
            axis.title = element_text(hjust=0.5, lineheight = 1, size = rel(0.9)),
            axis.title.y = element_blank())
  },
  aspect_ratio = 1/1.1,
  title = "Young women are more likely than young men to be economically inactive and not in school",
  subtitle = "Share of female youth population (ages 15-24) not in employment, education or training, most recent year available during 2010-14 (%)",
  source = "Source: International Labour Organization Key Indicators of the Labour Market database; WDI (SL.UEM.NEET.FE.ZS, SL.UEM.NEET.MA.ZS)",
  source_url = "http://datatopics.worldbank.org/sdgatlas/sdg8#neet"
)}


#' @export
wbgcharts_demo_atlas_4k_area_panels <- function() {figure(
  data = function() {
    df <- wbgdata(country = "all", indicator = c("SE.PRM.UNER"),
                  startdate = 1990, enddate = 2014, removeNA = TRUE)

    # impute EAS 1997
    df.regions <- df %>% filter(iso3c %in% wbgref$regions$iso3c)
    df.regions<- rbind(df.regions, data.frame(
      iso3c = "EAS",
      date = 1997,
      SE.PRM.UNER = mean(
        df$SE.PRM.UNER[df$iso3c == "EAS" & df$date == 1996],
        df$SE.PRM.UNER[df$iso3c == "EAS" & df$date == 1998]
      )
    ))

    # Tidy up country data set
    df.countries <- df %>%
      filter(iso3c %in% wbgref$countries$iso3c) %>%
      left_join(wbgref$countries$regions)

    # Get MRV for each country in >= 2010
    df.countries <- df.countries %>%
      group_by(iso3c, region_iso3c) %>%
      filter(date == max(date) & date >= 2010) %>%
      ungroup

    # And we'll only break out relatively large countries
    df.countries <- df.countries %>%
      arrange(-SE.PRM.UNER) %>%
      filter(SE.PRM.UNER > 2.5e6) %>%
      mutate(country = TRUE)

    df.countries.totals <- df.countries %>%
      select(-date) %>%
      group_by(region_iso3c) %>%
      summarise(breakout_sum = sum(SE.PRM.UNER))

    # Now some dplyr magic to create a combined dataset
    df.mrv_breakout <- df.regions %>%
      filter(date == 2014) %>%
      mutate(region_iso3c = iso3c, country=FALSE) %>%
      left_join(df.countries.totals) %>%
      mutate(breakout_sum = ifelse(is.na(breakout_sum), 0, breakout_sum)) %>%
      mutate(SE.PRM.UNER = SE.PRM.UNER - breakout_sum) %>%
      mutate(iso3c = ifelse(breakout_sum > 0, paste0(iso3c, ".rest"), iso3c)) %>%
      select(-breakout_sum) %>%
      bind_rows(df.countries)

    # Finally we need to sort in display order, that is by countries by value, then rest
    iso3c_sorted <- df.mrv_breakout %>%
      arrange(region_iso3c, -grepl(".rest", iso3c), SE.PRM.UNER) %>%
      pull(iso3c)

    df.mrv_breakout <- df.mrv_breakout %>%
      mutate(iso3c = factor(iso3c, levels = iso3c_sorted)) %>%
      arrange(iso3c) %>%
      mutate(
        cum_value = rev(cumsum(rev(SE.PRM.UNER))),
        label_y = (cum_value + lead(cum_value, default = 0))/2
      )
    data = list(out_of_school = df.regions, mrv_breakout = df.mrv_breakout)
  },
  plot = function(data, style = style_atlas()) {
    p <- ggplot(data$out_of_school, aes(x = date, y = SE.PRM.UNER, fill = iso3c)) +
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

    p.bar <- ggplot(data$mrv_breakout, aes(
        x = "2010-14",
        y = SE.PRM.UNER,
        fill=region_iso3c,
        group=iso3c
      )) +
      geom_col(position = "stack") +
      geom_hline(
        data = data$mrv_breakout %>% filter(country),
        mapping = aes(yintercept = cum_value),
        color="white", size = 0.35) +
      geom_text(
        data = data$mrv_breakout %>% filter(country),
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
    chart$theme <- style$theme()
    chart
  },
  aspect_ratio = 3/2,
#  theme = style_atlas()$theme(),
  title = "The number of children out of school fell across regions",
  subtitle = "Primary school-age children out of school, by region (millions)",
  source = "Source: United Nations Educational Scientific and Cultural Organization Institute for Statistics; WDI (SE.PRM.UNER).",
  note = "Note: 1997 East Asia & Pacific imputed.",
  source_url = "http://datatopics.worldbank.org/sdgatlas/sdg4#outofschool"
)}

#' @export
wbgcharts_demo_atlas_17c_line <- function() {figure(
  data = function() {
    df <- wbgdata(country = "LIC", indicator = "BX.KLT.DINV.WD.GD.ZS",
                  startdate = 2000, enddate = 2015)
    df
  },
  plot = function(df, style=style_atlas()) {
    ggplot(df, aes(x = date, y = BX.KLT.DINV.WD.GD.ZS)) +
      geom_line(color = style$colors$spot.primary, size = 1) +
      scale_x_continuous(expand = c(0,0)) +
      style$theme()
  },
  aspect_ratio = 1/2,
  title = "FDI flows to low-income countries have been falling since 2011 and stood at around 4 percent of GDP in 2015...",
  subtitle = "FDI, net inflows to low-income countries (% of GDP)",
  source = "Source: IMF, International Financial Statistics and Balance of Payments databases; World Bank, International Debt Statistics; World Bank and OECD GDP extimates; WDI (BX.KLT.DINV.WD.GD.ZS).",
  source_url = "http://datatopics.worldbank.org/sdgatlas/sdg17#fdi"
)}

#' @export
wbgcharts_demo_atlas_all <- function(path, style = style_atlas) {
  figure_save_allformats(wbgcharts_demo_atlas_1a_panel, paste0(path, "1a"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_2a_stackedarea, paste0(path, "2a"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_2b_dot, paste0(path, "2b"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_4k_area_panels, paste0(path, "4k"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_6f_dot, paste0(path, "6f"), style = style)
  figure_save_allformats(wbgcharts_demo_atlas_8c_scatter, paste0(path, "8c"), style = style)
}
