

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