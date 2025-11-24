library(tidyverse)
library(sf)
library(giscoR)
library(stringdist)
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
briefPlotLänder = left_join(wahlkreise, länderArt[länderArt$Jahr == 2025 & länderArt$Wahlbezirksart == "Brief",], by = c("WKR_NR" = "Wahlkreis-Nr."))
briefPlotLänder$meistgewählt = factor(
  briefPlotLänder$meistgewählt,
  c("1", "2", "3", "4", "5", "6", "7"),
  c("CDU", "CSU", "SPD", "LINKE", "GRÜNE", "FDP", "AFD")
)

ggplot(data = briefPlotLänder) + 
  geom_sf(aes(fill = (meistgewählt))) + 
  scale_fill_manual(
    name = "Partei",
    values = c("CDU" = "black", "CSU" = "#343A40", "SPD" = "red", "LINKE" = "pink", "GRÜNE" = "green", "FDP" = "yellow", "AFD" = "lightblue"),
    ) +
  theme_minimal() +
  labs(
    title = "Bundestagswahl 2025",
    subtitle = "Briefwähler"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.text = element_text(size = 8),
  )