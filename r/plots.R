library(tidyverse)
library(sf)
library(giscoR)
library(stringdist)
source("main.r")

kreise = gisco_get_nuts(
  country = "Germany",
  nuts_level = 3,
  year = "2024",
  epsg = 3035,
  resolution = "10"
)

wahlkreise = st_read("../data/wahlkreise/wahlkreise.shp")

bundesländer = gisco_get_nuts(
  country = "Germany",
  nuts_level = 1,
  year = "2024",
  epsg = 3035,
  resolution = "10"
)

kreiseReinigen = function(kreise) {
  kreise %>%
    tolower() %>%
    gsub("andkreis|kreisfreie stadt|kreis|,|- ", "", .) %>%
    trimws()
}

kreise$clean_name = kreiseReinigen(kreise$NAME_LATN)
kreis_daten_gesamt$clean_name = kreiseReinigen(kreis_daten_gesamt$Wahlkreisname)

matched_names = sapply(kreis_daten_gesamt$clean_name, function(name) {
  which.min(stringdist::stringdist(name, kreise$clean_name, method = "jw"))
})

kreis_daten_gesamt$NUTS_ID = kreise$NUTS_ID[matched_names]
kreis_daten_gesamt_sf = merge(kreise, kreis_daten_gesamt, by = "NUTS_ID")

plot_kreis_daten = function(column, title, legend_title) {
  ggplot(data = kreis_daten_gesamt_sf) +
    geom_sf(aes_string(fill = column), color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = legend_title) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
}

plot1 = plot_kreis_daten("Wähler", "Wähler nach Wahlkreis 2025", "Wähler")
# Unfinished 
plot(plot1)

bund_daten = left_join(bundesländer, kreis_daten_gesamt, by = c("NUTS_NAME" = "Land"))
bund_daten = bund_daten[bund_daten$Jahr == 2025,] %>%
  group_by(NUTS_NAME) %>%
  summarise(Wähler = sum(Wähler))

plot2 = ggplot(data = bund_daten) +
  geom_sf(aes(fill = Wähler), color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Wähler") +
  labs(title = "Wähler nach Bundesland 2025") +
  theme_minimal()
  
plot(plot2)

