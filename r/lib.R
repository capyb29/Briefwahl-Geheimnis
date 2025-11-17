loadcsv = function(file, uselessrows) {
  df = read.csv2(paste0("../data/", file), skip=uselessrows)
  df = df[,colSums(is.na(df)) == 0]
  colnames(df) = df[1,]
  df = df[-1,]
  rownames(df) = NULL
  #print(paste0("NA Values: ", any(is.na(df))))
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
  #Ich hasse 2025
  if (deparse(substitute(df)) == "kreis25") {
    for (i in seq_len(nrow(df_new))) {
      #Vergleicht die Wahlkreisnummer aus dem neuen Dataframe mit der aus dem Original und nimmt dann die Wahlberechtigtenanzahl aus der Urnen Zeile
      urne_wahlberechtigte = which(as.integer(df_new$`Wahlkreis-Nummer`[i]) == as.integer(df$`Wahlkreis-Nummer`) & df$Wahlbezirksart == "Urne")
      if (length(urne_wahlberechtigte) > 0) {
        df_new$Wahlberechtigte[i] = df$Wahlberechtigte[urne_wahlberechtigte]
      }
    }
  } else {
    # Das gleiche für Jahre 2017,2021
    df_new = df_new[-1,]
    for (i in seq_len(nrow(df_new))) {
      urne_wahlberechtigte = which(as.integer(df_new$`Wahlkreis-Nr.`[i]) == as.integer(df$`Wahlkreis-Nr.`) & df$Wahlbezirksart == "Urne")
      if (length(urne_wahlberechtigte) > 0) {
        df_new$Wahlberechtigte[i] = df$Wahlberechtigte[urne_wahlberechtigte]
      }
    }
  }
  df_new$Wahlbezirksart = NULL
  
  df_new = change_col_classes(df_new, c("numeric", "character", "character", rep("numeric", ncol(df_new)-3)))
  return(df_new)
}

change_col_classes = function(df, new_classes) {
  if (length(new_classes) != ncol(df)) {
    stop("Length of new_classes must match number of columns in data frame")
  }
  for (i in seq_along(new_classes)) {
    target_class = new_classes[i]
    # Convert each column based on target_class
    df[[i]] = switch(target_class,
                      character = as.character(df[[i]]),
                      numeric = as.numeric(df[[i]]),
                      integer = as.integer(df[[i]]),
                      factor = as.factor(df[[i]]),
                      logical = as.logical(df[[i]]),
                      # fallback: try to coerce using as.<class>()
                      do.call(paste0("as.", target_class), list(df[[i]]))
    )
  }
  return(df)
}

rbind_fill_na = function(df_big, df_small) {
  missing_cols = setdiff(names(df_big), names(df_small))
  
  for (col in missing_cols) {
    df_small[[col]] = 0
  }
  
  df_small = df_small[, names(df_big)]
  
  combined = rbind(df_big, df_small)
  return(combined)
}

