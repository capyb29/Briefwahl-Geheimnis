library(tidyverse)
library(shiny)
library(leaflet)
library(stringdist)
library(sf)
library(htmltools)

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

briefPlotLänder_longlat = st_transform(briefPlotLänder, crs = 4326)

bbox = st_bbox(briefPlotLänder_longlat)




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

shinyApp(ui, server)