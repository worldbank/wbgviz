Clean water and sanitation
==========================

``` r
  library(wbstats)
  library(tidyr)
  library(knitr)
  source("sdg-atlas.R")

  df_long <- wb(country = "1W",
                indicator = c("SH.H2O.SAFE.ZS","SH.STA.ACSN","SP.POP.TOTL","SH.STA.ODFC.ZS"),
                startdate=1990, enddate=2015)
  df_long$indicator <- NULL
  df <- spread(df_long, indicatorID, value)
  
  checks <- data.frame(Value = unlist(list(
    "World access to improved water sources"
    = df$SH.H2O.SAFE.ZS[df$date == 2015],
    
    "Increase in population with access to sanitation"
    = df$SH.STA.ACSN[df$date == 2015]/100 * df$SP.POP.TOTL[df$date == 2015] - 
      df$SH.STA.ACSN[df$date == 1990]/100 * df$SP.POP.TOTL[df$date == 1990],
    
    "Open defecation, 1990"
    = df$SH.STA.ODFC.ZS[df$date == 1990],
  
    "Open defecation, 2015"
    = df$SH.STA.ODFC.ZS[df$date == 2015]
  )))
#  kable(data.frame(Value = prettyNum(unlist(checks), big.mark = ",")), caption="#facttable")
  kable(checks, caption="#facttable", format.args = list(big.mark = ",", scientific=FALSE))
```

|                                                  |                Value|
|--------------------------------------------------|--------------------:|
| World access to improved water sources           |             90.97255|
| Increase in population with access to sanitation |  2,166,290,486.53577|
| Open defecation, 1990                            |             26.51903|
| Open defecation, 2015                            |             13.34539|

[Download data](sdg6_files/figure-markdown_github/sdg6-intro-1.csv)

Ensure availability and sustainable management of water and sanitation for all
------------------------------------------------------------------------------

More than 90 percent of the world’s people now have access to improved water sources. In the past 25 years 2.1 billion people gained access to improved sanitation facilities. At the same time the share of people practicing open defecation halved, from 27 percent to 13 percent. While such improvements show progress toward access for all, these measures do not capture all dimensions of providing water and sanitation. Goal 6 introduces a new, more comprehensive monitoring framework to ensure access that is safe, equitable, and universal.

### Expanding access to drinking water and sanitation

Until recently countries reported their populations’ access to water and sanitation by distinguishing between “improved” and “unimproved” coverage. In 2015, 663 million people were drinking from unimproved sources such as unprotected dug wells, and 2.4 billion lacked improved sanitation facilities. The bulk of those without were in Sub-Saharan Africa and South Asia (see figures 6d and 6e on page 34), where rural dwellers, especially the poorest, lagged behind others in access to both water and sanitation (figures 6a, 6b, 6f and 6g).

``` r
################################################################################################## #
# Analysis ####################################################################################### #
####################################################################################################

library(wbstats)
library(tidyr)
source("wdi-helper.R")

indicators <- c("SH.H2O.SAFE.RU.ZS", "SH.H2O.SAFE.UR.ZS","SP.URB.TOTL", "SP.RUR.TOTL")
df <- wb(country=wdi_regions, indicator=indicators, startdate=1990, enddate=2015)
df$indicator <- NULL

df_wide <- spread(df, indicatorID, value)
df_wide$rural <- (1 - df_wide$SH.H2O.SAFE.RU.ZS/100.0) * df_wide$SP.RUR.TOTL
df_wide$urban <- (1 - df_wide$SH.H2O.SAFE.UR.ZS/100.0) * df_wide$SP.URB.TOTL
df_wide[,indicators] <- NULL
df <- gather(df_wide, "urbanrural", "no_access", rural, urban)

df$group <- ifelse(df$iso2c %in% c("Z7","ZJ","XU","ZQ"), "Other regions combined", paste0(df$country, ", ", df$urbanrural))
df <- aggregate(no_access ~ date + group, data=df, FUN=sum)

################################################################################################## #
# Plotting ####################################################################################### #
####################################################################################################

library(ggplot2)
source("sdg-atlas.R")
source("geom_area_interactive.R")
library(directlabels)

group_order <- rev(c("Sub-Saharan Africa, rural", "Sub-Saharan Africa, urban","South Asia, rural",
                 "South Asia, urban", "East Asia & Pacific, rural", "East Asia & Pacific, urban","Other regions combined"))
df$group <- factor(df$group, group_order)

p <- ggplot(data = df, aes(x = as.numeric(date), y = no_access, fill = group, label = group)) + 
#  geom_area(position="stack", aes(x = as.numeric(date), y = no_access, fill = group)) + 
  geom_area_interactive(position="stack", value_header="(millions)", values=function(n) {round(n/1e6)}) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(0e8,0.5e9,1e9,1.5e9), labels = c("0.0", "0.5", "1.0", "1.5"), limits=c(0e9,1.5e9)) +
  scale_fill_manual(values=colors_atlas_regions(group_order)) +
  labs(title="The number of people without access to an improved water source is declining",
       subtitle="Number of people without access to an improved water source (billions)",
       caption="Source: World Health Organization/United Nations Children's Fund Joint Monitoring Programme for Water Supply and Sanitation; \nWDI (SH.H2O.SAFE.RU.ZS, SH.H2O.SAFE.UR.ZS,SP.URB.TOTL, SP.RUR.TOTL).",
       x="",y="") +
  geom_dl(position="stack", color="#111111", method=list(lightcolor="#f8f8f8", cex=0.7, "area.bl")) +
  # geom_atlas_labels(
  #   x = c(1990.5,2001,1990.5,1999,1990.5,2002,1990.5),
  #   y = c(0.075,0.2,0.35,0.5,0.675,0.8,1.15)*1e9,
  #   x.to = c(NA,2001.6,NA,1999.6,NA,2002.6,NA),
  #   y.to = c(NA,0.29,NA,0.56,NA,0.87,NA)*1e9,
  #   label = rev(group_order),
  #   color = c("#575756","#575756","white","white","white","white","#575756"),
  #   hjust = c(0,1,0,1,0,1,0),
  #   size = 3
  # ) +
  theme_atlas()

if (grepl("html", knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
  ggiraph(code = {print(p)},width=1,
          width_svg = opts_current$get("fig.width"), height_svg = opts_current$get("fig.height"),
          tooltip_opacity=1.0, tooltip_extra_css="background-color: white; border: 1px solid grey; padding: 7px; border-radius: 5px; box-shadow: rgba(0, 0, 0, 0.3) 0 2px 10px;")
} else {
  print(p)  
}
```

![](sdg6_files/figure-markdown_github/sdg6-wateraccess-1.png)

    ##      fill x         y                      label PANEL group      ymax
    ## 1 #FFCC00 0 0.1945694  Sub-Saharan Africa, rural     1     7 0.1945694
    ## 2 #C39F00 0 0.2087976  Sub-Saharan Africa, urban     1     6 0.2087976
    ## 3 #2478B6 0 0.3849163          South Asia, rural     1     5 0.3849163
    ## 4 #1D5B89 0 0.4033116          South Asia, urban     1     4 0.4033116
    ## 5 #DF7F22 0 0.6977648 East Asia & Pacific, rural     1     3 0.6977648
    ## 6 #A66117 0 0.7127571 East Asia & Pacific, urban     1     2 0.7127571
    ##   xmin xmax       ymin  colour size angle hjust vjust alpha rot
    ## 1    0    0 0.04545455 #111111    5     0   0.5   0.5     1   0
    ## 2    0    0 0.19456938 #111111    5     0   0.5   0.5     1   0
    ## 3    0    0 0.20879763 #111111    5     0   0.5   0.5     1   0
    ## 4    0    0 0.38491633 #111111    5     0   0.5   0.5     1   0
    ## 5    0    0 0.40331159 #111111    5     0   0.5   0.5     1   0
    ## 6    0    0 0.69776481 #111111    5     0   0.5   0.5     1   0
    ##                       groups
    ## 1  Sub-Saharan Africa, rural
    ## 2  Sub-Saharan Africa, urban
    ## 3          South Asia, rural
    ## 4          South Asia, urban
    ## 5 East Asia & Pacific, rural
    ## 6 East Asia & Pacific, urban

[Download data](sdg6_files/figure-markdown_github/sdg6-wateraccess-1.csv)

``` r
################################################################################################## #
# Analysis ####################################################################################### #
####################################################################################################

library(wbstats)
library(tidyr)
source("wdi-helper.R")

indicators <- c("SH.STA.ACSN.RU", "SH.STA.ACSN.UR","SP.URB.TOTL", "SP.RUR.TOTL")
df <- wb(country=wdi_regions, indicator=indicators, startdate=1990, enddate=2015)
df$indicator <- NULL

df_wide <- spread(df, indicatorID, value)
df_wide$rural <- (1 - df_wide$SH.STA.ACSN.RU/100.0) * df_wide$SP.RUR.TOTL
df_wide$urban <- (1 - df_wide$SH.STA.ACSN.UR/100.0) * df_wide$SP.URB.TOTL
df_wide[,indicators] <- NULL
df <- gather(df_wide, "urbanrural", "no_access", rural, urban)

df$group <- ifelse(df$iso2c %in% c("Z7","ZJ","XU","ZQ"), "Other regions combined", paste0(df$country, ", ", df$urbanrural))
df <- aggregate(no_access ~ date + group, data=df, FUN=sum)

################################################################################################## #
# Plotting ####################################################################################### #
####################################################################################################

library(ggplot2)
source("sdg-atlas.R")

group_order <- rev(c("Sub-Saharan Africa, rural", "Sub-Saharan Africa, urban","South Asia, rural",
                 "South Asia, urban", "East Asia & Pacific, rural", "East Asia & Pacific, urban","Other regions combined"))
df$group <- factor(df$group, group_order)

p <- ggplot(df) + 
#  geom_area(position="stack", aes(x = as.numeric(date), y = no_access, fill = group)) + 
  geom_area_interactive(position="stack", aes(x = as.numeric(date), y = no_access, fill = group, label = group), value_header="(millions)", values=function(n) {round(n/1e6)}) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(breaks = c(0,1e9,2e9,3e9), labels = c(0,1,2,3), limits=c(0e9,3e9)) +
  scale_fill_manual(values=colors_atlas_regions(group_order)) +
  labs(title="2.4 billion people still lack access to improved sanitation facilities",
       subtitle="Number of people without access to improved sanitation facilities (billions)",
       caption="Source: World Health Organization/United Nations Children's Fund Joint Monitoring Programme for Water Supply and Sanitation; \nWDI (SH.STA.ACSN.RU, SH.STA.ACSN.UR, SP.URB.TOTL, SP.RUR.TOTL).",
       x="",y="") +
  geom_atlas_labels(
    x = c(1990.5,2001,1990.5,1999,1990.5,2001,1990.5),
    y = c(0.15,0.25,0.55,1.1,1.5,1.9,2.35)*1e9,
    x.to = c(NA,2001.6,NA,1999.6,NA,2001.6,NA),
    y.to = c(NA,0.45,NA,1.4,NA,2.15,NA)*1e9,
    label = rev(group_order),
    color = c("#575756","#575756","white","white","white","white","#575756"),
    hjust = c(0,1,0,1,0,1,0),
    size = 3
  ) +  theme_atlas()

if (grepl("html", knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
  ggiraph(code = {print(p)},width=1,
          width_svg = opts_current$get("fig.width"), height_svg = opts_current$get("fig.height"),
          tooltip_opacity=1.0, tooltip_extra_css="background-color: white; border: 1px solid grey; padding: 7px; border-radius: 5px; box-shadow: rgba(0, 0, 0, 0.3) 0 2px 10px;")
} else {
  print(p)  
}
```

![](sdg6_files/figure-markdown_github/sdg6-saniaccess-1.png)

[Download data](sdg6_files/figure-markdown_github/sdg6-saniaccess-1.csv)

### Measuring access more comprehensively

Goal 6 commits to universal access to water, sanitation, and hygiene under a new, broader, and more refined monitoring framework (targets 6.1 and 6.2). The unimproved–improved distinction is replaced by “safely managed” services. For water, this requires that the household’s drinking water source is on premises, available when needed, and free of fecal and locally relevant chemical contaminants. For sanitation, emphasis is on the links in the sanitation chain from initial defecation through waste management (including containment, disposal, and transport of human excreta), and on the availability of an appropriate handwashing facility. Monitoring these components and inequalities will help assess progress toward the longer term aim of universal access.

Household surveys will increasingly measure these new components, but at present, data are limited. Available data show how the refined methodology can affect measures of access. For example, in Niger 66 percent of the population has access to an improved source of water, but new data show that only 10 percent have access on premises (figure 6c). Those without on-premise access must plan ahead to collect water, an exercise that can take up to 30 minutes (the threshold for “basic water”—another measure sometimes used) or even longer (improved, but not basic).

Even if data are not yet available on all aspects of safely managed water, there generally is information on improved water on premises, and access to safely managed water can be no higher than that. On average across six countries, 60 percent of urban dwellers and 75 percent of rural dwellers previously classified as having access would now be considered to be without access. When other new requirements of access are considered, the shares are likely to fall further.[1] These refined measures can help quantify major issues invisible in previous definitions.

Incorporating handwashing in the definition of sanitation access has a similar impact. A 54-country study found that the handwashing criterion was unmet for between 4 percent (Serbia, and Bosnia and Herzegovina) and 99 percent (Liberia and Ethiopia) of the population.[2] In another study of 10 countries, access to cleansing materials—fundamental to women for menstrual hygiene management—was below 25 percent in more than half the countries.[3] Such data[4] can give new insights to sanitation challenges facing different populations and enable countries and the international community to refine and focus service provision.[5]

``` r
################################################################################################## #
# Analysis ####################################################################################### #
####################################################################################################
library(tidyr)

df_wide <- data.frame(
  country = c("Pakistan","West Bank & Gaza","Nigeria","Niger","Congo, Dem. Rep.","Haiti"),
  improved = c(89,62,61,67,51,65),
  basic = c(87,62,45,46,37,55),
  iwop = c(77,61,31,10,8,7)
)
df_wide$country <- factor(df_wide$country, df_wide$country, ordered=T)

df <- gather(df_wide, access_type, value, improved, basic, iwop)
df$access_type <- factor(df$access_type, levels=c("improved", "basic", "iwop","safe"), labels=c("Improved water source","Basic water","Improved water on premises","(c) Safely managed water"), ordered=T)

################################################################################################## #
# Plotting ####################################################################################### #
####################################################################################################

library(ggplot2)
library(ggiraph)
source("sdg-atlas.R")

p <- ggplot(df, aes(x=country, y=value, fill=access_type, tooltip=paste(value, '%'))) + 
  geom_bar_interactive(width = 0.75,position=position_dodge(width=0.8), linetype="solid", stat="identity") +
#  geom_bar(width = 0.75,position=position_dodge(width=0.8), linetype="solid", stat="identity") +
  scale_fill_manual(values = c("#575756","#E69595","#CC0641","white"), drop=F) +
  scale_y_continuous(limits=c(0,100)) +
  labs(title="Under stricter definitions, fewer people have access to water",
       subtitle="Share of population at each access level, according to latest dataset, by country (%)",
       caption="a. Differs from the WDI indicator SH.H2O.SAFE.ZS, which is based on multiple surveys.\nb. Improved water source, with no more than a 30-minute round-trip collection time.\nc. Safely managed water access has not yet been assessed and is not shown but can be no greater than improved water on premises.\nSource: World Bank WASH Poverty Diagnostics 2016.",
       x="",y="") +
  theme_atlas() +
  theme(legend.direction = "horizontal", legend.position = "top", legend.title = element_blank())

if (grepl("html", knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
  ggiraph(code = {print(p)},width=1,
          width_svg = opts_current$get("fig.width"), height_svg = opts_current$get("fig.height"),
          tooltip_opacity=1.0, tooltip_extra_css="background-color: white; border: 1px solid grey; padding: 7px; border-radius: 5px; box-shadow: rgba(0, 0, 0, 0.3) 0 2px 10px;")
} else {
  print(p)  
}
```

![](sdg6_files/figure-markdown_github/sdg-newmeasure-1.png)

[Download data](sdg6_files/figure-markdown_github/sdg-newmeasure-1.csv)

<script>HTMLWidgets.addPostRenderHandler(function() { $('[data-id="geom_xstripe_interactive"]').attr("class", "area_sensor") })</script>

[1] The Water Supply, Sanitation, and Hygiene (WASH) Poverty Diagnostic has been working to examine the implications of adopting the new measurements of access.

[2] WHO/UNICEF Joint Monitoring Programme (JMP) for Water Supply and Sanitation, 2015, Progress on Sanitation and Drinking Water—2015 Update and MDG Assessment.

[3] Loughnan, L., Bain, R., Rop, R., Sommer, M., and Slaymaker, T. 2016. “What Can Existing Data on Water and Sanitation Tell Us About Menstrual Hygiene Management?” Waterlines 35(3).

[4] The most comprehensive assessment of the baseline global situation under these new measurements will be released by the World Health Organization/United Nations Children’s Fund Joint Monitoring Programme in 2017–18.

[5] World Bank WASH Poverty Diagnostic 2016.
