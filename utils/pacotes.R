print("instalando e carregando pacotes requeridos...")
packages <- c("shiny",
              "rmarkdown",
              "DBI",
              "rgdal",
              "tidygeocoder",
              "XML",
              "sidrar",
              "odbc",
              "pandoc",
              "raster",
              "cartography",
              "osmdata",
              "shinythemes",
              "shinyWidgets",
              "leaflet",
              "RColorBrewer",
              "htmlwidgets",
              "stringr",
              "rgeos",
              "ggplot2",
              "scales",
              "plotly",
              "magrittr",
              "dplyr")

install.packages(setdiff(packages, rownames(installed.packages())))  

# Carregando pacontes
lapply(packages,library,character.only = T)
