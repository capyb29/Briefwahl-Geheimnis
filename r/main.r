library(tidyverse)
source("lib.R")

bund17 = loadcsv("2017_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 10)
kreis17 = loadcsv("2017_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund21 = loadcsv("2021_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
kreis21 = loadcsv("2021_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund25 = loadcsv("2025_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
kreis25 = loadcsv("2025_Urne_Brief_Wahlkreise_Parteien.csv", 3)

# returns total amount of voters for given parameters
getWählerAmount = function(df, bezirksart, geschlecht, geburtsjahr, ez) { #only for bund dataframes
  df = df[df$Bezirksart == bezirksart & paste0(df$Geschlecht,"test") == paste0(geschlecht, "test") & 
                df$Geburtsjahresgruppe == geburtsjahr & df$`Erst-/Zweitstimme` == ez, "Summe"]
  return(as.numeric(df))
}

briefAnteile = function() {
    df = data.frame(
      Jahr = c("2017", "2021", "2025"),
      `Wähler gesamt` = c(
        getWählerAmount (bund17, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund21, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund25, "Summe", "Summe", "Summe", "2")
      ),
      männlich = c(
        getWählerAmount(bund17, "Summe", "m", "Summe", "2"),
        getWählerAmount(bund21, "Summe", "m", "Summe", "2"),
        getWählerAmount(bund25, "Summe", "m|d|o", "Summe", "2")
      ),
      weiblich = c(
        getWählerAmount(bund17, "Summe", "w", "Summe", "2"),
        getWählerAmount(bund21, "Summe", "w", "Summe", "2"),
        getWählerAmount(bund25, "Summe", "w", "Summe", "2")
      ),
      Briefwähler = c(
        getWählerAmount(bund17, "Brief", "Summe", "Summe", "2"),
        getWählerAmount(bund21, "Brief", "Summe", "Summe", "2"),
        getWählerAmount(bund25, "Brief", "Summe", "Summe", "2")
      ),
      Urnenwähler = c(
        getWählerAmount(bund17, "Urne", "Summe", "Summe", "2"),
        getWählerAmount(bund21, "Urne", "Summe", "Summe", "2"),
        getWählerAmount(bund25, "Urne", "Summe", "Summe", "2")
      ),
      `Anteil der Briefwähler` = c(
        getWählerAmount(bund17, "Brief", "Summe", "Summe", "2") / getWählerAmount(bund17, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund21, "Brief", "Summe", "Summe", "2") / getWählerAmount(bund21, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund25, "Brief", "Summe", "Summe", "2") / getWählerAmount(bund25, "Summe", "Summe", "Summe", "2")
      ),
      `Anteil der Urnenwähler` = c(
        getWählerAmount(bund17, "Urne", "Summe", "Summe", "2") / getWählerAmount(bund17, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund21, "Urne", "Summe", "Summe", "2") / getWählerAmount(bund21, "Summe", "Summe", "Summe", "2"),
        getWählerAmount(bund25, "Urne", "Summe", "Summe", "2") / getWählerAmount(bund25, "Summe", "Summe", "Summe", "2")
      )
    )
    rownames(df) = df$Jahr
    df$Jahr = NULL
    return((df))
}

correctBundesLänder = function(df) {
  bundeslaender <- c(
    HH = "Hansestadt Hamburg",
    HB = "Hansestadt Bremen",
    SH = "Schleswig-Holstein",
    MV = "Mecklenburg-Vorpommern",
    BE = "Berlin",
    BB = "Brandenburg",
    BY = "Bayern",
    BW = "Baden-Württemberg",
    HE = "Hessen",
    NI = "Niedersachsen",
    NW = "Nordrhein-Westfalen",
    RP = "Rheinland-Pfalz",
    SL = "Saarland",
    SN = "Sachsen",
    ST = "Sachsen-Anhalt",
    TH = "Thüringen"
  )
  df$Land = bundeslaender[df$Land]
  if (is.na(df$Land[1])) {
    df$Land[1] = ""
  }
  return(df)
}


# Anteil der Briefwähler pro Wahljahr
übersichtAnteilBriefwähler = briefAnteile()
savecsv(übersichtAnteilBriefwähler, "Anteil_Briefwähler.csv")

kreis17_zweitstimmen = getZweitstimmenKreis(kreis17)
kreis17_zweitstimmen = correctBundesLänder(kreis17_zweitstimmen)
savecsv(kreis17_zweitstimmen, "Kreis17_Zweitstimmen.csv")
kreis21_zweitstimmen = getZweitstimmenKreis(kreis21)
kreis21_zweitstimmen = correctBundesLänder(kreis21_zweitstimmen)
savecsv(kreis21_zweitstimmen, "Kreis21_Zweitstimmen.csv")
kreis25_zweitstimmen = getZweitstimmenKreis(kreis25)
kreis25_zweitstimmen = correctBundesLänder(kreis25_zweitstimmen)
savecsv(kreis25_zweitstimmen, "Kreis25_Zweitstimmen.csv")
