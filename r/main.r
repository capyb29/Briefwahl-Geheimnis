

loadcsv = function(file, uselessrows) {
  df = read.csv(paste0("../data/", file), sep = ";")
  df = df[,colSums(is.na(df)) == 0]
  df = df[-(1:uselessrows),]
  colnames(df) = df[1,]
  df = df[-1,]
  rownames(df) = NULL
  return(df)
}

btw2025 = loadcsv("btw2025_bezirk.csv", 4)

btw2021kreis = loadcsv("btw2021kreis.csv", 3)
btw2021kreis = btw2021kreis[-1,] # offensichtlich, dass alles erststimmen bei der kreiswahl sind

btwUrneBrief = loadcsv("btw_urne-brief_1957_2025.csv", 3)
btwUrneBrief = btwUrneBrief[btwUrneBrief$`Jahr der Wahl` >= 2017, ]
rownames(btwUrneBrief) = NULL

btwUrneBrief[,4:ncol(btwUrneBrief)] = lapply(btwUrneBrief[,4:ncol(btwUrneBrief)], function(x) as.numeric(gsub("\\.", "", btwUrneBrief$Wählende)))

anzahlBrief = function(Land, Jahr) {
  df = btwUrneBrief[btwUrneBrief$Land == Land & btwUrneBrief$`Jahr der Wahl` == Jahr, ]
  if (nrow(df) == 2) {
    a = df[df$Wahlbezirksart == "Brief", "Wählende"] /
             sum(df$Wählende)
    return(round(a, digits=3))
  }
}

anteil = anzahlBrief("BW", 2025)
cat(anteil * 100)
