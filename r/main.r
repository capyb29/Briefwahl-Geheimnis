

loadcsv = function(file, uselessrows) {
  df = read.csv(paste0("../data/", file), skip=uselessrows, sep=";")
  df = df[,colSums(is.na(df)) == 0]
  colnames(df) = df[1,]
  df = df[-1,]
  rownames(df) = NULL
  print(paste0("NA Values: ", any(is.na(df))))
  return(df)
}

savecsv = function(df, file) {
  write.csv2(df, paste0("../data/", file), row.names = TRUE)
}

bund17 = loadcsv("2017_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 10)
kreis17 = loadcsv("2017_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund21 = loadcsv("2021_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
kreis21 = loadcsv("2021_Urne_Brief_Wahlkreise_Parteien.csv", 3)
bund25 = loadcsv("2025_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 11)
kreis25 = loadcsv("2025_Urne_Brief_Wahlkreise_Parteien.csv", 3)
