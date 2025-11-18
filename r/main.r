library(tidyverse)
library(sf)
library(giscoR)
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

correctBundesLänder = function(df) {
  bundeslaender <- c(
    HH = "Hamburg",
    HB = "Bremen",
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

briefwähler_länder17 = briefwähler_länder(kreis17_zweitstimmen)
briefwähler_länder17$Jahr = 2017
briefwähler_länder21 = briefwähler_länder(kreis21_zweitstimmen)
briefwähler_länder21$Jahr = 2021
briefwähler_länder25 = briefwähler_länder(kreis25_zweitstimmen)
briefwähler_länder25$Jahr = 2025

briefwähler_länder_gesamt = rbind(briefwähler_länder17,
                                  briefwähler_länder21,
                                  briefwähler_länder25)

bund = bundDatenBereinigen(bund17, bund21, bund25)
bund_komplett = bund[!grepl("Summe", bund$Bezirksart), ]
bund_komplett = bund_komplett[!grepl("Summe", bund_komplett$Geschlecht), ]
bund_komplett = bund_komplett[!grepl("Summe", bund_komplett$Geburtsjahresgruppe), ]
rownames(bund_komplett) = NULL
bund_komplett = change_col_classes(bund_komplett, c("numeric", rep("character", 3), rep("numeric", ncol(bund_komplett) -
                                                                                          4)))
bund3 = bund[grepl("Summe", bund$Bezirksart), ]
bund3 = rbind(bund3, bund[grepl("Summe", bund$Geschlecht), ])
bund3 = rbind(bund3, bund[grepl("Summe", bund$Geburtsjahresgruppe), ])
bund3 = unique(bund3)
rownames(bund3) = NULL

savecsv(bund, "Bund.csv")
savecsv(bund_komplett, "Bund_clean.csv")
savecsv(bund3, "Bund_sums.csv")

# cleanup kreisdaten
kreis_daten_gesamt = kreisdatenBereinigen(kreis17_zweitstimmen,
                                          kreis21_zweitstimmen,
                                          kreis25_zweitstimmen)

savecsv(kreis_daten_gesamt, "Kreisdaten_Gesamt.csv")

res = bundAnalyse(group = c("Jahr", "Geschlecht"))
# Wahlbeteiligung nach Wahlbezirksart in den Gruppen

filters = c("Jahr", "Land")

res2 = kreis_daten_gesamt[, ] %>% group_by(across(all_of(filters))) %>%
  summarise(
    Wahlberechtigte = kreisWahlberechtigteByGroup(cur_group()),
    Wahlbeteiligung = pct(sum(Wähler) / kreisWahlberechtigteByGroup(cur_group())),
    
  )