library(tidyverse)
source("lib.R")

bund17 = loadcsv("2017_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 10)
kreis17 = loadcsv("2017_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund21 = loadcsv("2021_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
kreis21 = loadcsv("2021_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund25 = loadcsv("2025_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
kreis25 = loadcsv("2025_Urne_Brief_Wahlkreise_Parteien.csv", 3)

getWählerAmount = function(df, bezirksart, geschlecht, geburtsjahr, ez) { #only for bund dataframes
  return( as.numeric(df[df$Bezirksart == bezirksart & df$Geschlecht == geschlecht & 
                df$Geburtsjahresgruppe == geburtsjahr & df$`Erst-/Zweitstimme` == ez, "Summe"]))
}

briefAnteile = function() {
  return(
    data.frame(
      Jahr = c("2017", "2021", "2025"),
      `Wähler gesamt` = c(
        getWählerAmount (bund17, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund21, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund25, "Summe", "Summe", "Summe", "2")
      ),
      `Anteil der Briefwähler` = c(
        getWählerAmount(bund17, "Brief", "Summe", "Summe", "2") / getWählerAmount(bund17, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund21, "Brief", "Summe", "Summe", "2") / getWählerAmount(bund21, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund25, "Brief", "Summe", "Summe", "2") / getWählerAmount(bund25, "Summe", "Summe", "Summe", "2")
      )
    )
  )
}

# Anteil der Briefwähler pro Wahljahr
übersichtAnteilBriefwähler = briefAnteile