library(tidyverse)
source("lib.R")

bund17 = loadcsv("2017_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 10)
kreis17 = loadcsv("2017_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund21 = loadcsv("2021_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
kreis21 = loadcsv("2021_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund25 = loadcsv("2025_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
# Bei 2025 m|d|o Geschlecht zu m geändert 
bund25$Geschlecht[bund25$Geschlecht == "m|d|o"] = "m"
kreis25 = loadcsv("2025_Urne_Brief_Wahlkreise_Parteien.csv", 3)
# Spaltennamen anpassen
names(kreis25)[names(kreis25) == "Wählende"] = "Wähler"
names(kreis21)[names(kreis21) == "Wählende"] = "Wähler"


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
        getWählerAmount(bund25, "Summe", "m", "Summe", "2")
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

# Daten Bereinigen und Zusammenfassen
bundDatenBereinigen = function(df,df2,df3) {
  df = df[df$`Erst-/Zweitstimme` == 2,]
  df2 = df2[df2$`Erst-/Zweitstimme` == 2,]
  df3 = df3[df3$`Erst-/Zweitstimme` == 2,]
  names(df3)[names(df3) == "Die Linke"] = "DIE LINKE"
  df$Jahr = 2017
  df2$Jahr = 2021
  df3$Jahr = 2025
  vec_tmp = c(colnames(df),colnames(df2),colnames(df3))
  vec_tmp = unique(vec_tmp)
  vec_tmp = c("Jahr", vec_tmp[vec_tmp != "Jahr"])
  df_new = data.frame(matrix(ncol = length(vec_tmp), nrow = 0))
  colnames(df_new) = vec_tmp
  df_new = rbind_fill_na(df_new, df)
  df_new = rbind_fill_na(df_new, df2)
  df_new = rbind_fill_na(df_new, df3)
  df_new$`Erst-/Zweitstimme` = NULL
  rownames(df_new) = NULL
  return(df_new)
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

#Anteile der Briefwähler in den Ländern
briefwähler_länder = function(df) {
   res = df %>% group_by(Land) %>% 
    summarise(
      Wahlberechtigte = sum(Wahlberechtigte), 
      Wähler = sum(Wähler),
      Anteil_Briefwähler = round(sum(Wähler) / sum(Wahlberechtigte), 3) * 100
    )
   
   summary = list(Land = "Bund", Wahlberechtigte = sum(res$Wahlberechtigte), Wähler = sum(res$Wähler), Anteil_Briefwähler = round(sum(res$Wähler) / sum(res$Wahlberechtigte), 3)*100)
   summary = as.data.frame(summary)
   #res = rbind(res, summary)
  return(res)
}

briefwähler_länder17 = briefwähler_länder(kreis17_zweitstimmen)
briefwähler_länder17$Jahr = 2017
briefwähler_länder21 = briefwähler_länder(kreis21_zweitstimmen)
briefwähler_länder21$Jahr = 2021
briefwähler_länder25 = briefwähler_länder(kreis25_zweitstimmen)
briefwähler_länder25$Jahr = 2025

briefwähler_länder_gesamt = rbind(briefwähler_länder17, briefwähler_länder21, briefwähler_länder25)


bund = bundDatenBereinigen(bund17, bund21, bund25)
savecsv(bund, "Bund_combined.csv")