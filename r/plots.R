library(tidyverse)
library(sf)
library(giscoR)
library(stringdist)
source("main.r")

wahlkreise = st_read("../data/wahlkreise/wahlkreise.shp")

bundesländer = gisco_get_nuts(
  country = "Germany",
  nuts_level = 1,
  year = "2024",
  epsg = 3035,
  resolution = "10"
)

#briefPlotLänder17 = left_join(bundesländer, länderArt[länderArt$Jahr == 2017 & länderArt$Wahlbezirksart == "Brief",], by = c("NUTS_NAME" = "Land"))
briefPlotLänder = left_join(wahlkreise, länderArt[länderArt$Jahr == 2017 & länderArt$Wahlbezirksart == "Urne",], by = c("WKR_NR" = "Wahlkreis-Nr."))

wahlkreisPlot = ggplot(data = briefPlotLänder) + 
  geom_sf(aes(fill = (meistgewählt))) + 
  scale_fill_manual(
    name = "Partei",
    values = c("1" = "black", "2" = "#343A40", "3" = "red", "4" = "pink", "5" = "green", "6" = "yellow", "7" = "lightblue"),
    labels = c("CDU", "CSU", "SPD", "LINKE", "AFD")
    ) +
  theme_minimal() +
  labs(
    title = "Bundestagswahl 2017",
    subtitle = "Urnenwähler"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.text = element_text(size = 8),
  )

plot(wahlkreisPlot)


