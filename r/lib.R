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
  write.csv2(df, paste0("../data/processed/", file), row.names = TRUE)
}

# Filtert die Zweitstimmen Briefwahl-Daten für Kreise heraus
getZweitstimmenKreis = function(df) {
  #Filtern für Zweitstimmen/Briefwahl
  if (deparse(substitute(df)) == "kreis25") {
    df_new = df[,!grepl("Erststimmen", colnames(df))]
  } else {
    df_new = df[, df[1,]!="Erststimmen"]
  }
  df_new = df_new[df_new$Wahlbezirksart !="Urne",]
  
  # Hinzufügen der Wahlberechtigten aus Urnenwahlbezirken
  df_tmp = df_new
  #Ich hasse 2025
  if (deparse(substitute(df)) == "kreis25") {
    for (i in seq_len(nrow(df_tmp))) {
      #Vergleicht die Wahlkreisnummer aus dem neuen Dataframe mit der aus dem Original und nimmt dann die Wahlberechtigtenanzahl aus der Urnen Zeile
      urne_wahlberechtigte = which(as.integer(df_tmp$`Wahlkreis-Nummer`[i]) == as.integer(df$`Wahlkreis-Nummer`) & df$Wahlbezirksart == "Urne")
      if (length(urne_wahlberechtigte) > 0) {
        df_new$Wahlberechtigte[i] = df$Wahlberechtigte[urne_wahlberechtigte]
      }
    }
  } else {
    # Das gleiche für Jahre 2017,2021
    df_tmp = df_tmp[-1,]
    for (i in seq_len(nrow(df_tmp))) {
      urne_wahlberechtigte = which(as.integer(df_tmp$`Wahlkreis-Nr.`[i]) == as.integer(df$`Wahlkreis-Nr.`) & df$Wahlbezirksart == "Urne")
      if (length(urne_wahlberechtigte) > 0) {
        df_new$Wahlberechtigte[i+1] = df$Wahlberechtigte[urne_wahlberechtigte]
      }
    }
  }
  return(df_new)
}