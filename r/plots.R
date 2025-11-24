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

# Anteil der Briefwähler von allen Wahlberechtigten
wahlkreise = st_read("../data/wahlkreise/wahlkreise.shp")
plot1 = left_join(wahlkreise, res2, by = c("WKR_NR" = "Wahlkreis-Nr."))
plot1 = plot1[!is.na(plot1$Wahlbeteiligung), ]

ggplot(data = plot1) +
  geom_sf(aes(fill = Wahlbeteiligung)) +
  scale_fill_gradient(low = "red", high = "green", name = "Briefwähler\nin %\n") +
  labs(title = "Bundestagswahl 2025\nAnteil der Briefwähler\nvon allen Wahlberechtigten") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),
    legend.title = element_text(hjust = 0.5, size = 8),
    legend.text = element_text(size = 10)
  )


bundesländer = gisco_get_nuts(
  country = "Germany",
  nuts_level = 1,
  year = "2024",
  epsg = 3035,
  resolution = "10"
)

#briefPlotLänder17 = left_join(bundesländer, länderArt[länderArt$Jahr == 2017 & länderArt$Wahlbezirksart == "Brief",], by = c("NUTS_NAME" = "Land"))
briefPlotLänder = left_join(wahlkreise, länderArt[länderArt$Jahr == 2025 & länderArt$Wahlbezirksart == "Brief",], by = c("WKR_NR" = "Wahlkreis-Nr."))

ggplot(data = briefPlotLänder) + 
  geom_sf(aes(fill = (meistgewählt))) + 
  scale_fill_manual(
    name = "Partei",
    values = c("1" = "black", "2" = "#343A40", "3" = "red", "4" = "pink", "5" = "green", "6" = "yellow", "7" = "lightblue"),
    labels = c("CDU", "CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AFD")
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

