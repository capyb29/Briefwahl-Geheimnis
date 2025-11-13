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