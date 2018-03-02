# WBGviz - comprising `wbgcharts`, `wbggeo` and `wbgmaps`

This repo is a collection of packages for standardising visualizations for (initially) the WDI and SDG Atlas. It comprises three packages:
- wbgcharts - a variety of extensions to `ggplot2` for figures styling, and output
- wbggeo - specific extensions for map output (not needed if not producing maps)
- wbgmaps - a package containing WBG-compliant mapping base layers (rendering borders, etc as per [Style Guide](http://documents.worldbank.org/curated/en/154921467999692668/World-Bank-editorial-style-guide) Appendix H). These are already bundled into a format `ggplot` understands, so no supporting geospatial libraries are needed.

Note that if you are producing maps for official World Bank publications it is _extremely_ important to use the `wbgmaps` basemaps.

## Installation (NOT TESTED)

Each of the three packages must be installed separately. Steps from within R/RStudio:

1. Make sure you have `devtools` installed from CRAN: `install.packages('devtools')`
2. (While this repo is private) You need to generated a Github access token to install R packages from private repos:
  - Go to https://github.com/settings/tokens/new
  - Enter something in description and check the [x] repo box.
  - Choose generate token
  - Take a note of the token [SECRET] - you won't be able to see it again (although you can always delete it and generate another)
3. Use devtools to install each package in turn:
```
devtools::install_github("worldbank/wbgviz", subdir = "wbgcharts", auth_token = "[SECRET]")
devtools::install_github("worldbank/wbgviz", subdir = "wbggeo", auth_token = "[SECRET]")
devtools::install_github("worldbank/wbgviz", subdir = "wbgmaps", auth_token = "[SECRET]")
```
Note that by default `install_github` does not create vignettes - you can enable this by adding an argument `build_vignettes = TRUE` to each of the three above commands. Then you can view the vignettes included in a package by e.g. `vignette(package="wbgcharts")`.

## Font installation

To use styles, you need to have font dependencies. One (Avenir) is a commerical license so we can't distribute with the package. Assuming it is already installed on your system, the following may work:
```
font_import()
```
This will take a while. When it completes, see if Avenir is now included in the font list by examing:
```
View(fonttable()
```
Chances are, it won't work, because Avenir is wrapped up in a `.ttc` file that `extrafont` doesn't recognise. Various tools will extract `.ttc` files into `.ttf` files, or you can ask @EconAndrew for the pre-extracted fonts (if you have a license).

If you don't have Avenir, `style_atlas_open` and `style_worldbank.org` substitute similar open fonts that can be installed using:
```
install.packages("extrafont")
font_import(system.file("fonts", package = "wbgcharts"))
```

You will probably need to install them at the OS level too. Usually this is by opening the path printed by
```
system.file("fonts", package = "wbgcharts")
```
then double clicking or right clicking on the fonts there.
