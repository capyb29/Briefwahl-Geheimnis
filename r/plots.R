library(tidyverse)
library(sf)
#library(giscoR)
library(stringdist)
library(leaflet)
library(shiny)
library(htmltools)
source("main.r")

#Ka ob wir das brauchen
# bundesländer = gisco_get_nuts(
#   country = "Germany",
#   nuts_level = 1,
#   year = "2024",
#   epsg = 3035,
#   resolution = "10"
# )

# Anteil der Briefwähler von allen Wahlberechtigten
wahlkreise = st_read("../data/wahlkreise/wahlkreise.shp")

länderArt = kreis_daten_gesamt[, ] %>% group_by(Jahr, `Wahlkreis-Nr.`, Wahlbezirksart) %>%
  summarise(
    Wahlkreisname,
    Wähler = sum(Wähler),
    Ungültige = sum(Ungültige),
    CDU = sum(CDU),
    CSU = sum(CSU),
    SPD = sum(SPD),
    LINKE = sum(`DIE LINKE`),
    GRÜNE = sum(GRÜNE),
    FDP = sum(FDP),
    AFD = sum(AfD)
  )
länderArt$meistgewählt = max.col(länderArt[, (ncol(länderArt) - 6):(ncol(länderArt) -
                                                                      0)])
länderArt$pct = apply(länderArt[, (6:13)], 1, "max")
länderArt$pct = pct(länderArt$pct / länderArt$Wähler)

# fügt die räumlichen daten mit den wahlkreis daten zusammen
briefPlotLänder = left_join(wahlkreise, länderArt, by = c("WKR_NR" = "Wahlkreis-Nr."))
briefPlotLänder$meistgewählt = factor(
  briefPlotLänder$meistgewählt,
  c("1", "2", "3", "4", "5", "6", "7"),
  c("CDU", "CSU", "SPD", "LINKE", "GRÜNE", "FDP", "AFD")
)

# schwarze magie bei gott ich weiß nicht was abgeht
temp_df = briefPlotLänder %>%
  rowwise() %>%
  mutate(
    ref_value = get(as.character(meistgewählt), as.list(cur_data()))
  ) %>%
  ungroup()

meist_vals = temp_df %>%
  group_by(WKR_NR, Jahr) %>%
  summarise(
    meist_brief = meistgewählt[Wahlbezirksart == "Brief"],
    value_brief = ref_value[Wahlbezirksart == "Brief"],
    meist_urne = meistgewählt[Wahlbezirksart == "Urne"],
    value_urne = ref_value[Wahlbezirksart == "Urne"],
    meistgewählt_total = ifelse(value_brief >= value_urne, meist_brief, meist_urne),
    .groups = "drop"
  )
briefPlotLänder = temp_df %>%
     left_join(
         meist_vals %>% st_drop_geometry() %>% select(WKR_NR, Jahr, meistgewählt_total),
         by = c("WKR_NR", "Jahr")
     )
#faktor für die meistgewählte partei insgesamt
briefPlotLänder$meistgewählt_total = factor(
  briefPlotLänder$meistgewählt_total,
  c("1", "2", "3", "4", "5", "6", "7"),
  c("CDU", "CSU", "SPD", "LINKE", "GRÜNE", "FDP", "AFD")
)

# Gibt den Parteien farben
party_colors = c(
  "CDU" = "black",
  "CSU" = "#343A40",
  "SPD" = "red",
  "LINKE" = "pink",
  "GRÜNE" = "green",
  "FDP" = "yellow",
  "AFD" = "blue"
)
pal = colorFactor(palette = party_colors, domain = briefPlotLänder$meistgewählt)

#wandelt in das longlat format um für leaflet
briefPlotLänder_longlat = st_transform(briefPlotLänder, crs = 4326)

#macht eine bbox damit man den größtnen udn kleinsten punkt der karte hat fürn zoom
bbox = st_bbox(briefPlotLänder_longlat)

# alles ui Funktionen für die kontrollelementenoben rechts
ui = bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", height = "100%", width = "100%"),
  absolutePanel(
    top = 10,
    width = "8vw",
    height = "auto",
    right = 5,
    style = "background-color: #f5f5f5; padding: 10px; border-radius: 5px;",
    radioButtons(
      "jahr",
      "Wahljahr",
      choices = c("2017", "2021", "2025"),
      selected = "2025"
    ),
    radioButtons(
      "art",
      "Wahlart",
      choices = c("Brief", "Urne", "Beides"),
      selected = "Beides"
    ),
    selectInput(
      "popup_mode",
      "Popup-Anzeige",
      choices = c(
        "Absolute Zahlen" = "Absolute",
        "Prozentuale Anteile" = "Prozent"
      ),
      selected = "Absolute"
    )
  )
)

server = function(input, output, session) {
  # reactive heißt er guckt auf änderungen in den input feldern und updated die daten dementsprechend
  gefilterte_daten = reactive({
    jahr = as.numeric(input$jahr)
    art = input$art
    daten = briefPlotLänder_longlat[briefPlotLänder_longlat$Jahr == jahr, ]
    if (art != "Beides") {
      daten = daten[daten$Wahlbezirksart == art, ]
    } else {
      daten$meistgewählt = daten$meistgewählt_total
      #parteien addieren für beides
      daten = daten %>% group_by(WKR_NR) %>%
        summarise(
          Wahlkreisname = first(Wahlkreisname),
          Wähler = sum(Wähler),
          CDU = sum(CDU),
          CSU = sum(CSU),
          SPD = sum(SPD),
          LINKE = sum(LINKE),
          GRÜNE = sum(GRÜNE),
          FDP = sum(FDP),
          AFD = sum(AFD),
          meistgewählt = first(meistgewählt)
        )
      #TODO fix oben stehendes
    }
    #TODO popup parteien nach stärke absteigend sortieren
    #Switch case guckt in der input popup_mode choice box welche option gewählt wurde
    switch (
      input$popup_mode,
      "Prozent" = (
        daten$popup_content = paste0(
          "<strong>Wahlkreis: </strong>",
          daten$Wahlkreisname,
          "<br/>",
          "<strong>Parteien: </strong><br/>",
          "CDU/CSU: ",
          pct((daten$CDU + daten$CSU) / daten$Wähler),
          "%<br/>",
          "SPD: ",
          pct(daten$SPD / daten$Wähler),
          "%<br/>",
          "LINKE: ",
          pct(daten$LINKE / daten$Wähler),
          "%<br/>",
          "GRÜNE: ",
          pct(daten$GRÜNE / daten$Wähler),
          "%<br/>",
          "FDP: ",
          pct(daten$FDP / daten$Wähler),
          "%<br/>",
          "AFD: ",
          pct(daten$AFD / daten$Wähler),
          "%<br/>"
        )
      ),
      "Absolute" = (
        daten$popup_content = paste0(
          "<strong>Wahlkreis: </strong>",
          daten$Wahlkreisname,
          "<br/>",
          "<strong>Parteien: </strong><br/>",
          "CDU/CSU: ",
          daten$CDU + daten$CSU,
          "<br/>",
          "SPD: ",
          daten$SPD,
          "<br/>",
          "LINKE: ",
          daten$LINKE,
          "<br/>",
          "GRÜNE: ",
          daten$GRÜNE,
          "<br/>",
          "FDP: ",
          daten$FDP,
          "<br/>",
          "AFD: ",
          daten$AFD,
          "<br/>"
        )
      )
    )
    daten
  })
  
  # rendert die map zum ersten mal
  output$map = renderLeaflet({
    leaflet(briefPlotLänder_longlat) %>%
      addTiles() %>%
      setView(lng = 10.45403,
              lat = 51.16425,
              zoom = 6) %>%
      addLegend(
        pal = pal,
        values = ~ meistgewählt,
        opacity = 0.7,
        title = "Partei",
        position = "bottomright"
      )
  })
  
  #observed ob sich die input daten geändert haben und updated dementsprechend die map
  observe({
    leafletProxy("map", data = gefilterte_daten()) %>%
      clearShapes() %>%
      #polygons zeichnen für die füllfläche
      addPolygons(
        fillColor = ~ pal(meistgewählt),
        group = ~ meistgewählt,
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
        # lqbels fürs drüber hovern
        label = ~ paste0(
          "<b>",
          Wahlkreisname,
          "</b><br/>",
          meistgewählt,
          " (",
          pct,
          "%)"
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          html = TRUE,
          direction = "auto"
        ),
        #popup inhalte
        popup = ~ popup_content
      )
  })
}

shinyApp(ui, server)

# Die Zeile unten in der R-Konsole!!! ausführen, um die Shiny-App zu starten
# runApp('plots.R')



# resBundAnalyse = bundAnalyse(group = "Bezirksart")
#
# x1 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Brief"]
# y1 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Brief"]
#
# x2 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Urne"]
# y2 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Urne"]
#
# plot(x1, y1, type = "b", frame = FALSE, pch = 19,
#      col = "red", xlab = "Jahr", ylab = "Anteil in %",
#      xlim = c(2016, 2026), ylim = c(0,100), main = "Anteil der Brief- und Urnenwahl pro Jahr")
#
# lines(x2, y2, pch = 19, col = "blue", type = "b", lty = 1)
# abline(y2[1], 0, col = "blue", lty = 2)
# abline(y1[1], 0, col = "red", lty = 2)
#
# text(2016.6, 33, paste(y1[1], "%"), col = "red", cex = 0.7)
# text(2016.6, 75.8, paste(y2[1], "%"), col = "blue", cex = 0.7)
#
# legend(2016, 103, legend=c("Brief", "Urne"),
#        col=c("red", "blue"), lty = 1, pch = 19, cex=0.9)
#
#
# plot_wahlbeteiligung_jahr = function(df, jahr) {
#   df_jahr = df %>% filter(Jahr == jahr) %>%
#     mutate(ba = (Wahlbezirksart == "Brief") * Anteil) %>%
#     arrange(desc(ba))
#
#   df_jahr$Land = factor(df_jahr$Land, levels = unique(df_jahr$Land))
#   df_jahr$Wahlbezirksart = factor(df_jahr$Wahlbezirksart, levels = c("Urne", "Brief"))
#
#   m = round(mean(df$Anteil[df$Wahlbezirksart == "Brief" & df$Jahr == jahr]),1)
#
#   # Plot erstellen mit gestapelten Balken für Urne/Brief je Bundesland
#   p = ggplot(df_jahr, aes(x = Land, y = Anteil, fill = Wahlbezirksart)) +
#     geom_bar(stat = "identity", position = "stack", width = 0.8) +
#     labs(
#       title = paste("Anteil Brief- und Urnenwahl in", jahr),
#       x = "Bundesland",
#       y = "Anteil (%)",
#       fill = "Wahlbezirksart"
#     ) +
#     geom_text(
#       aes(label = round(Anteil)),
#       position = position_stack(vjust = 0.5), size = 2.7, color = "white"
#     ) +
#     geom_hline(
#       yintercept = m,
#       color = "blue",
#       linetype = "dashed",
#       size = 1) +
#     annotate(
#       "text",
#       x = length(levels(df_jahr$Land)) / 2 -0.5,
#       y = m+7,
#       label = paste(m, "%"),
#       size = 5,
#       hjust = 0,
#       fontface = "bold",
#       color = "blue"
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1),
#           plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#           plot.subtitle = element_text(hjust = 0.5, size = 12),
#           legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
#           legend.text = element_text(size = 8))
#
#   print(p)
# }
#
# anteileLänderJahre = kreis_daten_gesamt %>% group_by(Jahr, Land, Wahlbezirksart) %>%
#   summarise(Anteil = pct(sum(Wähler) / sum(kreis_daten_gesamt[kreis_daten_gesamt$Jahr == Jahr & kreis_daten_gesamt$Land == Land, "Wähler"])))
#
# #briefwahl durschnitt nicht korrekt
#
# # Bayern und Rheinland-Pfalz immer oben mit dabei
# plot_wahlbeteiligung_jahr(anteileLänderJahre, 2017)
# ggsave("./plots/ArtAnteileLänder17.png")
# plot_wahlbeteiligung_jahr(anteileLänderJahre, 2021)
# ggsave("./plots/ArtAnteileLänder21.png")
# plot_wahlbeteiligung_jahr(anteileLänderJahre, 2025)
# ggsave("./plots/ArtAnteileLänder25.png")
