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

länderArt = kreis_daten_gesamt[,] %>% group_by(Jahr, `Wahlkreis-Nr.`, Wahlbezirksart) %>% 
  summarise(Wahlkreisname, Wähler = sum(Wähler), Ungültige = sum(Ungültige), CDU = sum(CDU), CSU = sum(CSU), SPD = sum(SPD), LINKE = sum(`DIE LINKE`), GRÜNE = sum(GRÜNE), FDP = sum(FDP), AFD = sum(AfD))
länderArt$meistgewählt = max.col(länderArt[,(ncol(länderArt)-6):(ncol(länderArt)-0)])
länderArt$pct = apply(länderArt[,(6:13)], 1, "max")
länderArt$pct = pct(länderArt$pct / länderArt$Wähler)

#briefPlotLänder17 = left_join(bundesländer, länderArt[länderArt$Jahr == 2017 & länderArt$Wahlbezirksart == "Brief",], by = c("NUTS_NAME" = "Land"))
briefPlotLänder = left_join(wahlkreise, länderArt[länderArt$Jahr == 2025 & länderArt$Wahlbezirksart == "Brief",], by = c("WKR_NR" = "Wahlkreis-Nr."))
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
  addControl(
    html = "<h3 style='text-align:center;'>Bundestagswahl 20xx</h3>",
    position = "bottomleft"
  ) %>%
  addPolygons(
    fillColor = ~pal(meistgewählt),
    group = ~meistgewählt,
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
      style = list("font-weight" = "normal", padding = "3px 8px"), 357
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~meistgewählt,
    opacity = 0.7,
    title = "Partei",
    position = "bottomright"
  ) %>%
  addLayersControl(overlayGroups = levels(briefPlotLänder$meistgewählt),
                   options = layersControlOptions(collapsed = FALSE))
interactive


# Plot Brief/Urnenanteil über Jahreart
artAnteilJahre = function() {
  resBundAnalyse = bundAnalyse(group = "Bezirksart")

  x1 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Brief"]
  y1 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Brief"]

  x2 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Urne"]
  y2 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Urne"]

  plot(x1, y1, type = "b", frame = FALSE, pch = 19, 
      col = "red", xlab = "Jahr", ylab = "Anteil in %", 
      xlim = c(2016, 2026), ylim = c(0,100), main = "Anteil der Brief- und Urnenwahl pro Jahr")

  lines(x2, y2, pch = 19, col = "blue", type = "b", lty = 1)
  abline(y2[1], 0, col = "blue", lty = 2)
  abline(y1[1], 0, col = "red", lty = 2)

  text(2016.6, 33, paste(y1[1], "%"), col = "red", cex = 0.7)
  text(2016.6, 75.8, paste(y2[1], "%"), col = "blue", cex = 0.7)

  legend(2016, 103, legend=c("Brief", "Urne"),
        col=c("red", "blue"), lty = 1, pch = 19, cex=0.9)  
}

artAnteilJahre()

plot_wahlbeteiligung_jahr = function(df, jahr) {
  df_jahr = df %>% filter(Jahr == jahr) %>% 
    mutate(ba = (Wahlbezirksart == "Brief") * Anteil) %>%
    arrange(desc(ba))
  
  df_jahr$Land = factor(df_jahr$Land, levels = unique(df_jahr$Land))
  df_jahr$Wahlbezirksart = factor(df_jahr$Wahlbezirksart, levels = c("Urne", "Brief"))
  
  m = round(mean(df$Anteil[df$Wahlbezirksart == "Brief" & df$Jahr == jahr]),1)
  
  # Plot erstellen mit gestapelten Balken für Urne/Brief je Bundesland
  p = ggplot(df_jahr, aes(x = Land, y = Anteil, fill = Wahlbezirksart)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    labs(
      title = paste("Anteil Brief- und Urnenwahl in", jahr),
      x = "Bundesland",
      y = "Anteil (%)",
      fill = "Wahlbezirksart"
    ) +
    geom_text(
      aes(label = round(Anteil)), 
      position = position_stack(vjust = 0.5), size = 2.7, color = "white"
    ) +
    geom_hline(
      yintercept = m, 
      color = "blue", 
      linetype = "dashed", 
      size = 1) +
    annotate(
      "text",
      x = length(levels(df_jahr$Land)) / 2 -0.5, 
      y = m+7,            
      label = paste(m, "%"),
      size = 5,
      hjust = 0,
      fontface = "bold",
      color = "blue"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          legend.text = element_text(size = 8))
  
  print(p)
}

anteileLänderJahre = kreis_daten_gesamt %>% group_by(Jahr, Land, Wahlbezirksart) %>%
  summarise(Anteil = pct(sum(Wähler) / sum(kreis_daten_gesamt[kreis_daten_gesamt$Jahr == Jahr & kreis_daten_gesamt$Land == Land, "Wähler"])))

#briefwahl durschnitt nicht korrekt

# Bayern und Rheinland-Pfalz immer oben mit dabei
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2017)
ggsave("./plots/ArtAnteileLänder17.png")
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2021)
ggsave("./plots/ArtAnteileLänder21.png")
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2025)
ggsave("./plots/ArtAnteileLänder25.png")


# Plot Geschlecht Art Partei
bundArtGeschlecht = bundAnalyse(group = c("Bezirksart", "Geschlecht"))
parties = c("CDU_CSU","SPD","GRÜNE","LINKE","FDP","AFD","Sonstige")
df_long = bundArtGeschlecht %>% pivot_longer(cols = all_of(parties), names_to = "Partei", values_to = "Stimmenanteil")

df_long <- df_long %>%
  group_by(Jahr, Bezirksart, Geschlecht) %>%
  mutate(Partei = factor(Partei, levels = Partei[order(-Stimmenanteil)])) %>%
  ungroup()

ggplot(df_long) +
  aes(x = Partei, y = Stimmenanteil, fill = Geschlecht) + 
  labs(title = "Wahlanteile nach Bezirksart und Geschlecht über die Jahre") +
  geom_col(position = position_dodge()) + 
  facet_grid(Jahr ~ Bezirksart) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 8))
ggsave("./plots/ParteiArtGeschlechtJahre.png")

# vorläufig
# Plot with Brief and Urne side by side, stacked by Geschlecht
ggplot(bundArtGeschlecht, aes(x = factor(Jahr), y = Bezirksart_Anteil, fill = Geschlecht)) +
  geom_bar(aes(group = Bezirksart ), stat = "identity", position = position_stack()) +
  facet_wrap( ~Bezirksart) +
  labs(x = "Year", y = "Bezirksart Anteil", fill = "Geschlecht",
       title = "Bezirksart Anteile nach Geschlecht über die Jahre") +
  theme_minimal() +
  scale_x_discrete(expand = expansion(add = c(0.2, 0.2))) +
  theme(legend.position = "top")

# hypothesentest ob wähler von geschlecht abhängt,
# aktuell noch nicht ideal, da altersgruppen mit drin sind
# am besten mit newData nochmal machen, weil mehr datensätze
# dran denken alter rausnehmen
tBrief = t.test(Summe ~ Geschlecht, data = bund_komplett[bund_komplett$Bezirksart == "Brief",])
tUrne = t.test(Summe ~ Geschlecht, data = bund_komplett[bund_komplett$Bezirksart == "Urne",])


#nur ein test
# Create boxplot
ggplot(bund_komplett, aes(x = Bezirksart, y = Summe, fill = Bezirksart)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.5) + # show individual points
  labs(title = "Wähler",
       x = "Bezirksart",
       y = "Wähler") +
  theme_minimal() +
  theme(legend.position = "none")




