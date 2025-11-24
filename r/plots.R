library(tidyverse)
library(sf)
library(giscoR)
library(stringdist)
library(leaflet)
source("main.r")

# Anteil der Briefwähler von allen Wahlberechtigten
wahlkreise = st_read("../data/wahlkreise/wahlkreise.shp")

bundesländer = gisco_get_nuts(
  country = "Germany",
  nuts_level = 1,
  year = "2024",
  epsg = 3035,
  resolution = "10"
)

#briefPlotLänder17 = left_join(bundesländer, länderArt[länderArt$Jahr == 2017 & länderArt$Wahlbezirksart == "Brief",], by = c("NUTS_NAME" = "Land"))
briefPlotLänder = left_join(wahlkreise, länderArt[länderArt$Jahr == 2021 & länderArt$Wahlbezirksart == "Brief",], by = c("WKR_NR" = "Wahlkreis-Nr."))
briefPlotLänder$meistgewählt = factor(
  briefPlotLänder$meistgewählt,
  c("1", "2", "3", "4", "5", "6", "7"),
  c("CDU", "CSU", "SPD", "LINKE", "GRÜNE", "FDP", "AFD")
)

#interactive
briefPlotLänder_longlat <- st_transform(briefPlotLänder, crs = 4326)

# Create a color palette function based on your party colors
party_colors <- c(
  "CDU" = "black",
  "CSU" = "#343A40",
  "SPD" = "red",
  "LINKE" = "pink",
  "GRÜNE" = "green",
  "FDP" = "yellow",
  "AFD" = "lightblue"
)

pal <- colorFactor(palette = party_colors, domain = briefPlotLänder$meistgewählt)

# Leaflet interactive map
interactive = leaflet(briefPlotLänder_longlat) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(
    fillColor = ~pal(meistgewählt),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(Wahlkreisname, ": ", pct, "%"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~meistgewählt,
    opacity = 0.7,
    title = "Partei",
    position = "bottomright"
  )
interactive
