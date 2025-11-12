

loadcsv = function(file, uselessrows) {
  df = read.csv(paste0("../data/", file), sep = ";")
  df = df[,colSums(is.na(df)) == 0]
  df = df[-(1:uselessrows),]
  colnames(df) = df[1,]
  df = df[-1,]
  rownames(df) = NULL
  return(df)
}

savecsv = function(df, file) {
  write.csv2(df, paste0("../data/", file), row.names = TRUE)
}

btw2025 = loadcsv("btw2025_bezirk.csv", 4)

btw2021kreis = loadcsv("btw2021kreis.csv", 3)
btw2021kreis = btw2021kreis[-1,] # offensichtlich, dass alles erststimmen bei der kreiswahl sind

btwUrneBrief = loadcsv("btw_urne-brief_1957_2025.csv", 3)
btwUrneBrief = btwUrneBrief[btwUrneBrief$`Jahr der Wahl` >= 1994, ]
rownames(btwUrneBrief) = NULL

# convert cols to numeric
btwUrneBrief[,4:ncol(btwUrneBrief)] = lapply(btwUrneBrief[,4:ncol(btwUrneBrief)], function(x) as.numeric(gsub("\\.", "", btwUrneBrief$Wählende)))

anzahlBrief = function(Land, Jahr) {
  df = btwUrneBrief[btwUrneBrief$Land == Land & btwUrneBrief$`Jahr der Wahl` == Jahr, ]
  if (nrow(df) == 2) {
    a = df[df$Wahlbezirksart == "Brief", "Wählende"] /
             sum(df$Wählende)
    return(round(a, digits=3))
  }
}

AnteileBriefwähler = data.frame(
  Land = unique(btwUrneBrief$Land),
  Anteil_Brief_2017 = sapply(unique(btwUrneBrief$Land), function(x) anzahlBrief(x, 2017) * 100),
  Anteil_Brief_2021 = sapply(unique(btwUrneBrief$Land), function(x) anzahlBrief(x, 2021) * 100),
  Anteil_Brief_2025 = sapply(unique(btwUrneBrief$Land), function(x) anzahlBrief(x, 2025) * 100)
)
rownames(AnteileBriefwähler) = AnteileBriefwähler$Land
AnteileBriefwähler$Land = NULL

savecsv(AnteileBriefwähler, "Anteile_Briefwaehler.csv")

