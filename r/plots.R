library(tidyverse)
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

#Betiligung nach Art in Kreis

betArtKreis = kreis_daten_gesamt %>% group_by(Jahr, `Wahlkreis-Nr.`, Wahlbezirksart) %>% summarise(Wähler = sum(Gültige))
ichhörediestimme = kreis_daten_gesamt %>% group_by(Jahr, `Wahlkreis-Nr.`) %>% summarise(Stimmen = sum(Gültige))
betArtKreis = betArtKreis %>% left_join(ichhörediestimme, by = c("Jahr", "Wahlkreis-Nr."))
betArtKreis$artBeteiligung = pct(betArtKreis$Wähler / betArtKreis$Stimmen)

plotBetArt = function(df) {
  df = left_join(wahlkreise, df, by = c("WKR_NR" = "Wahlkreis-Nr."))
  ggplot(df) +
    geom_sf(aes(fill = artBeteiligung)) +
    scale_fill_gradient(
      low = "red",
      high = "green",
      limits = c(10 + (unique(
        df$Wahlbezirksart == "Urne"
      ) * 20), 70 + (unique(
        df$Wahlbezirksart == "Urne"
      ) * 20))
    ) +
    labs(
      title = paste0("Bundestagswahl ", unique(df$Jahr)),
      subtitle = paste("Wahlbezirksart:", unique(df$Wahlbezirksart)),
      fill = "Beteiligung (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(
        hjust = 0.5,
        size = 12,
        face = "bold"
      ),
      legend.text = element_text(size = 8)
    )
}

plotBetArt(betArtKreis[betArtKreis$Jahr == 2017 &
                         betArtKreis$Wahlbezirksart == "Brief", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2021 &
                         betArtKreis$Wahlbezirksart == "Brief", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2025 &
                         betArtKreis$Wahlbezirksart == "Brief", ])

plotBetArt(betArtKreis[betArtKreis$Jahr == 2017 &
                         betArtKreis$Wahlbezirksart == "Urne", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2021 &
                         betArtKreis$Wahlbezirksart == "Urne", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2025 &
                         betArtKreis$Wahlbezirksart == "Urne", ])


bundesländer = gisco_get_nuts(
  country = "Germany",
  nuts_level = 1,
  year = "2024",
  epsg = 3035,
  resolution = "10"
)

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
  mutate(ref_value = get(as.character(meistgewählt), as.list(cur_data()))) %>%
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
          party_name = as.character(first(meistgewählt)),
          pct = {
            data_grp <- cur_data()
            total_wähler <- sum(data_grp$Wähler)
            party_votes <- switch(
              party_name,
              "CDU" = sum(data_grp$CDU + data_grp$CSU),
              "CSU" = sum(data_grp$CDU + data_grp$CSU),
              "SPD" = sum(data_grp$SPD),
              "LINKE" = sum(data_grp$LINKE),
              "GRÜNE" = sum(data_grp$GRÜNE),
              "FDP" = sum(data_grp$FDP),
              "AFD" = sum(data_grp$AFD),
              NA_real_
            )
            pct(party_votes / total_wähler)
          },
          meistgewählt = first(meistgewählt)
        )
    }
    #Switch case guckt in der input popup_mode choice box welche option gewählt wurde
    switch (
      input$popup_mode,
      "Prozent" = (daten$popup_content = apply(daten, 1, function(row) {
        values = c(
          "CDU/CSU" = as.numeric(row["CDU"]) + as.numeric(row["CSU"]),
          "SPD" = as.numeric(row["SPD"]),
          "LINKE" = as.numeric(row["LINKE"]),
          "GRÜNE" = as.numeric(row["GRÜNE"]),
          "FDP" = as.numeric(row["FDP"]),
          "AFD" = as.numeric(row["AFD"])
        )
        
        total = as.numeric(row["Wähler"])
        values = pct(values / total)
        
        values = sort(values, decreasing = TRUE)
        
        partei_lines = paste0(names(values), ": ", values, if (input$popup_mode == "Prozent") {
          "%"
        } else {
          ""
        }, "<br/>", collapse = "")
        
        paste0(
          "<strong>Wahlkreis: </strong>",
          row["Wahlkreisname"],
          "<br/>",
          "<strong>Parteien: </strong><br/>",
          partei_lines
        )
      })),
      "Absolute" = (daten$popup_content = apply(daten, 1, function(row) {
        values = c(
          "CDU/CSU" = as.numeric(row["CDU"]) + as.numeric(row["CSU"]),
          "SPD" = as.numeric(row["SPD"]),
          "LINKE" = as.numeric(row["LINKE"]),
          "GRÜNE" = as.numeric(row["GRÜNE"]),
          "FDP" = as.numeric(row["FDP"]),
          "AFD" = as.numeric(row["AFD"])
        )
        
        values = sort(values, decreasing = TRUE)
        partei_lines = paste0(names(values), ": ", values, if (input$popup_mode == "Prozent") {
          "%"
        } else {
          ""
        }, "<br/>", collapse = "")
        paste0(
          "<strong>Wahlkreis: </strong>",
          row["Wahlkreisname"],
          "<br/>",
          "<strong>Parteien: </strong><br/>",
          partei_lines
        )
      }))
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


# Die Zeile unten in der R-Konsole!!! ausführen, um die Shiny-App zu starten
# runApp('plots.R')

artAnteilJahre = function() {
  resBundAnalyse = bundAnalyse(group = "Bezirksart")
  
  x1 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Brief"]
  y1 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Brief"]
  
  x2 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Urne"]
  y2 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Urne"]
  
  plot(
    x1,
    y1,
    type = "b",
    frame = FALSE,
    pch = 19,
    col = "red",
    xlab = "Jahr",
    ylab = "Anteil in %",
    xlim = c(2016, 2026),
    ylim = c(0, 100),
    main = "Anteil der Brief- und Urnenwahl pro Jahr"
  )
  
  lines(
    x2,
    y2,
    pch = 19,
    col = "blue",
    type = "b",
    lty = 1
  )
  abline(y2[1], 0, col = "blue", lty = 2)
  abline(y1[1], 0, col = "red", lty = 2)
  
  text(2016.6,
       33,
       paste(y1[1], "%"),
       col = "red",
       cex = 0.7)
  text(2016.6,
       75.8,
       paste(y2[1], "%"),
       col = "blue",
       cex = 0.7)
  
  legend(
    2016,
    103,
    legend = c("Brief", "Urne"),
    col = c("red", "blue"),
    lty = 1,
    pch = 19,
    cex = 0.9
  )
}

artAnteilJahre()

plot_wahlbeteiligung_jahr = function(df, jahr) {
  df_jahr = df %>% filter(Jahr == jahr) %>%
    mutate(ba = (Wahlbezirksart == "Brief") * Anteil) %>%
    arrange(desc(ba))
  
  
  m = kreis_daten_gesamt[kreis_daten_gesamt$Jahr == jahr, ] %>% group_by(Wahlbezirksart) %>%
    summarise(Anteil = pct(sum(Wähler) / sum(kreis_daten_gesamt$Wähler[kreis_daten_gesamt$Jahr == jahr])))
  m = m$Anteil[1]
  print(m)
  
  # Plot erstellen mit gestapelten Balken für Urne/Brief je Bundesland
  p = ggplot(df_jahr, aes(x = Land, y = Anteil, fill = Wahlbezirksart)) +
    geom_bar(stat = "identity",
             position = "stack",
             width = 0.8) +
    labs(
      title = paste("Anteil Brief- und Urnenwahl in", jahr),
      x = "Bundesland",
      y = "Anteil (%)",
      fill = "Wahlbezirksart"
    ) +
    geom_text(
      aes(label = round(Anteil)),
      position = position_stack(vjust = 0.5),
      size = 2.7,
      color = "white"
    ) +
    geom_hline(
      yintercept = m,
      color = "blue",
      linetype = "dashed",
      size = 1
    ) +
    annotate(
      "text",
      x = length(levels(df_jahr$Land)) / 2 - 0.5,
      y = m + 7,
      label = paste(m, "%"),
      size = 5,
      hjust = 0,
      fontface = "bold",
      color = "blue"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(
        hjust = 0.5,
        size = 12,
        face = "bold"
      ),
      legend.text = element_text(size = 8)
    )
  
  print(p)
}
anteileLänderJahre = kreis_daten_gesamt %>% group_by(Jahr, Land, Wahlbezirksart) %>%
  summarise(Anteil = pct(sum(Wähler) / sum(kreis_daten_gesamt[kreis_daten_gesamt$Jahr == Jahr &
                                                                kreis_daten_gesamt$Land == Land, "Wähler"])))

# Bayern und Rheinland-Pfalz immer oben mit dabei
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2017)
ggsave("./plots/ArtAnteileLänder17.png")
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2021)
ggsave("./plots/ArtAnteileLänder21.png")
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2025)
ggsave("./plots/ArtAnteileLänder25.png")


# Plot Geschlecht Art Partei
bundArtGeschlecht = bundAnalyse(group = c("Bezirksart", "Geschlecht"))
parties = c("CDU_CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AFD", "Sonstige")
df_long = bundArtGeschlecht %>% pivot_longer(cols = all_of(parties),
                                             names_to = "Partei",
                                             values_to = "Stimmenanteil")

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
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.text = element_text(size = 8)
  )
ggsave("./plots/ParteiArtGeschlechtJahre.png")

# vorläufig
# Plot with Brief and Urne side by side, stacked by Geschlecht
ggplot(bundArtGeschlecht,
       aes(x = factor(Jahr), y = Bezirksart_Anteil, fill = Geschlecht)) +
  geom_bar(aes(group = Bezirksart),
           stat = "identity",
           position = position_stack()) +
  facet_wrap(~ Bezirksart) +
  labs(x = "Year",
       y = "Bezirksart Anteil",
       fill = "Geschlecht",
       title = "Bezirksart Anteile nach Geschlecht über die Jahre") +
  theme_minimal() +
  scale_x_discrete(expand = expansion(add = c(0.2, 0.2))) +
  theme(legend.position = "top")

# hypothesentest ob wähler von geschlecht abhängt,
tBrief = t.test(Summe ~ Geschlecht, data = bund_komplett[bund_komplett$Bezirksart == "Brief", ])
tUrne = t.test(Summe ~ Geschlecht, data = bund_komplett[bund_komplett$Bezirksart == "Urne", ])

tTestRes = data.frame(Bezirksart = c("Brief", "Urne"),
                      `p-value` = c(tBrief$p.value, tUrne$p.value))

barplot(tTestRes$p.value, names = c("Brief", "Urne"), xlab = "Bezirksart", ylab = "P-Value", main = "Einfluss von Geschlecht auf die Summe der Wähler", col = c("#ffb703", "#023047"), ylim = c(0,1))
abline(0.05, 0, lty = 2)

# altersgruppen
gruppen = bund_komplett %>% group_by(Jahr, Bezirksart, Geburtsjahresgruppe) %>% summarise(
  Wähler = sum(Summe),
  CDU_CSU = sum(CDU) + sum(CSU),
  SPD = sum(SPD),
  GRÜNE = sum(GRÜNE),
  LINKE = sum(`DIE LINKE`),
  AFD = sum(AfD),
  FDP = sum(FDP)
)

gruppen17 = gruppen[gruppen$Jahr == 2017, ] %>% pivot_longer(cols = CDU_CSU:FDP,
                                                             names_to = "Partei",
                                                             values_to = "Stimmen")
gruppen21 = gruppen[gruppen$Jahr == 2021, ] %>% pivot_longer(cols = CDU_CSU:FDP,
                                                             names_to = "Partei",
                                                             values_to = "Stimmen")
gruppen25 = gruppen[gruppen$Jahr == 2025, ] %>% pivot_longer(cols = CDU_CSU:FDP,
                                                             names_to = "Partei",
                                                             values_to = "Stimmen")

ggplot(gruppen25) +                                                                                 ##
  aes(x = Geburtsjahresgruppe, y = Stimmen, fill = Partei) +                                        ##
  geom_bar(stat = "identity", position = "dodge") +                                                 ##
  scale_fill_manual(values = c(
    "#382BF0",
    "#17171C",
    "#FFE419",
    "#24C210",
    "#D92DC5",
    "#D92E2E"
  )) + ##
  facet_wrap( ~ Bezirksart) +                                                                         ##
  labs(
    x = "Geburtsjahresgruppe",
    y = "Anzahl Stimmen",
    fill = "Partei",
    ##
    title = "Stimmenanzahl nach Geburtsjahresgruppe, Partei und Bezirksart (2017)"
  ) +            ##
  theme_minimal() +                                                                                 ##
  theme(axis.text.x = element_text(angle = 45, hjust = 1))                                          ##

shinyApp(ui, server)


# Geschlechtsanteile pro Jahr

geschlechterAnteil = bund_komplett[bund_komplett$Bezirksart == "Brief",] %>% group_by(Jahr, Geschlecht) %>% summarise(Summe = sum(Summe)) %>% mutate(pct = Summe / sum(Summe))

pieGeschlecht = function(Jahr) {
  data = geschlechterAnteil[geschlechterAnteil$Jahr == Jahr,]
  pie(data$Summe, labels = c(paste0("männlich: ", pct(data$pct[data$Geschlecht == "m"]), "%"), paste0("weiblich: ", pct(data$pct[data$Geschlecht == "w"]), "%")), col = c("lightblue", "pink"), border = "white", main = paste0("Geschlechteranteil ", Jahr))
}
pieGeschlecht(2017)
pieGeschlecht(2021)
pieGeschlecht(2025)
