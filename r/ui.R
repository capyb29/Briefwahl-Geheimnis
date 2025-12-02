library(shiny)

bootstrapPage(
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
