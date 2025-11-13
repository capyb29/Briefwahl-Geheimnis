

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

bund17 = loadcsv("2017_Bund_Partei_Brief_Urne_Alter_Geschlecht.csv", 3)
