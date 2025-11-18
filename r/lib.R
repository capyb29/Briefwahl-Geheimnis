loadcsv = function(file, uselessrows) {
  df = read.csv2(paste0("../data/", file), skip = uselessrows)
  df = df[, colSums(is.na(df)) == 0]
  colnames(df) = df[1, ]
  df = df[-1, ]
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
    df_new = df[, !grepl("Erststimmen", colnames(df))]
  } else {
    df_new = df[, df[1, ] != "Erststimmen"]
  }
  
  # Hinzufügen der Wahlberechtigten aus Urnenwahlbezirken
  #Ich hasse 2025
  if (deparse(substitute(df)) == "kreis25") {
    for (i in seq_len(nrow(df_new))) {
      #Vergleicht die Wahlkreisnummer aus dem neuen Dataframe mit der aus dem Original und nimmt dann die Wahlberechtigtenanzahl aus der Urnen Zeile
      urne_wahlberechtigte = which(
        as.integer(df_new$`Wahlkreis-Nummer`[i]) == as.integer(df$`Wahlkreis-Nummer`) &
          df$Wahlbezirksart == "Urne"
      )
      if (length(urne_wahlberechtigte) > 0) {
        df_new$Wahlberechtigte[i] = df$Wahlberechtigte[urne_wahlberechtigte]
      }
    }
  } else {
    # Das gleiche für Jahre 2017,2021
    df_new = df_new[-1, ]
    for (i in seq_len(nrow(df_new))) {
      urne_wahlberechtigte = which(
        as.integer(df_new$`Wahlkreis-Nr.`[i]) == as.integer(df$`Wahlkreis-Nr.`) &
          df$Wahlbezirksart == "Urne"
      )
      if (length(urne_wahlberechtigte) > 0) {
        df_new$Wahlberechtigte[i] = df$Wahlberechtigte[urne_wahlberechtigte]
      }
    }
  }
  
  df_new = change_col_classes(df_new, c("numeric", rep("character", 3), rep("numeric", ncol(df_new) -
                                                                              4)))
  return(df_new)
}

change_col_classes = function(df, new_classes) {
  if (length(new_classes) != ncol(df)) {
    stop("Length of new_classes must match number of columns in data frame")
  }
  for (i in seq_along(new_classes)) {
    target_class = new_classes[i]
    # Convert each column based on target_class
    df[[i]] = switch(
      target_class,
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

pct = function(num) {
  return(round(num * 100, 1))
}

getWählerAmount = function(df,
                           bezirksart,
                           geschlecht,
                           geburtsjahr,
                           ez) {
  df = df[df$Bezirksart == bezirksart &
            paste0(df$Geschlecht, "test") == paste0(geschlecht, "test") &
            df$Geburtsjahresgruppe == geburtsjahr &
            df$`Erst-/Zweitstimme` == ez, "Summe"]
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

# Daten Bereinigen und Zusammenfassen
bundDatenBereinigen = function(df, df2, df3) {
  df = df[df$`Erst-/Zweitstimme` == 2, ]
  df2 = df2[df2$`Erst-/Zweitstimme` == 2, ]
  df3 = df3[df3$`Erst-/Zweitstimme` == 2, ]
  names(df3)[names(df3) == "Die Linke"] = "DIE LINKE"
  df$Jahr = 2017
  df2$Jahr = 2021
  df3$Jahr = 2025
  vec_tmp = c(colnames(df), colnames(df2), colnames(df3))
  vec_tmp = unique(vec_tmp)
  vec_tmp = c("Jahr", vec_tmp[vec_tmp != "Jahr"])
  df_new = data.frame(matrix(ncol = length(vec_tmp), nrow = 0))
  colnames(df_new) = vec_tmp
  df_new = rbind_fill_na(df_new, df)
  df_new = rbind_fill_na(df_new, df2)
  df_new = rbind_fill_na(df_new, df3)
  df_new$`Erst-/Zweitstimme` = NULL
  rownames(df_new) = NULL
  df_new$Land = NULL
  return(df_new)
}

kreisdatenBereinigen = function(df, df2, df3) {
  df$Jahr = 2017
  df2$Jahr = 2021
  df3$Jahr = 2025
  
  names(df2) = sub("du.", "du", names(df2))
  
  names(df3) = sub(" - Zweitstimmen$", "", names(df3))
  names(df3) = sub("Nummer", "Nr.", names(df3))
  names(df3) = sub("Die Linke", "DIE LINKE", names(df3))
  
  vec_tmp = c(colnames(df), colnames(df2), colnames(df3))
  vec_tmp = unique(vec_tmp)
  vec_tmp = c("Jahr", vec_tmp[vec_tmp != "Jahr"])
  vec_tmp = sub(".1", "", vec_tmp)
  df_new = data.frame(matrix(ncol = length(vec_tmp), nrow = 0))
  colnames(df_new) = vec_tmp
  df_new = rbind_fill_na(df_new, df)
  df_new = rbind_fill_na(df_new, df2)
  df_new = rbind_fill_na(df_new, df3)
  rownames(df_new) = NULL
  
  return(df_new)
}

#Anteile der Briefwähler in den Ländern
briefwähler_länder = function(df) {
  res = df %>% group_by(Land) %>%
    summarise(
      Wahlberechtigte = sum(Wahlberechtigte),
      Wähler = sum(Wähler),
      Anteil_Briefwähler = round(sum(Wähler) / sum(Wahlberechtigte), 3) * 100
    )
  
  summary = list(
    Land = "Bund",
    Wahlberechtigte = sum(res$Wahlberechtigte),
    Wähler = sum(res$Wähler),
    Anteil_Briefwähler = round(sum(res$Wähler) / sum(res$Wahlberechtigte), 3) *
      100
  )
  summary = as.data.frame(summary)
  #res = rbind(res, summary)
  return(res)
}

# ANALYSEN
kreisWahlberechtigteByGroup = function(filter_list) {
  df = kreis_daten_gesamt
  
  for (col in names(filter_list)) {
    val = filter_list[[col]]
    df = df %>% filter(.data[[col]] == val)
  }
  
  result = df %>%
    arrange(`Wahlkreis-Nr.`) %>%
    group_by(`Wahlkreis-Nr.`) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  sum(result$Wahlberechtigte)
}

bundSumByGroup = function(filter_list) {
  df = bund_komplett
  for (col in colnames(filter_list)) {
    val = filter_list[[col]]
    df = df %>% filter(.data[[col]] == val)
  }
  return(sum(df$Summe))
}

bundAnalyse = function(..., group = "Jahr") {
  args = list(...)
  
  res = bund_komplett
  for (col in names(args)) {
    val = args[[col]]
    res = res %>% filter(.data[[col]] == val)
  }
  
  res = res %>% group_by(across(all_of(group))) %>%
    summarise(
      Bezirksart_Anteil = pct(sum(Summe) / sum(bund_komplett[bund_komplett$Jahr == Jahr, "Summe"])),
      CDU_CSU = pct((sum(CDU) + sum(CSU)) / bundSumByGroup(cur_group())),
      SPD = pct(sum(SPD) / bundSumByGroup(cur_group())),
      GRÜNE = pct(sum(GRÜNE) / bundSumByGroup(cur_group())),
      LINKE = pct(sum(`DIE LINKE`) / bundSumByGroup(cur_group())),
      FDP = pct(sum(FDP) / bundSumByGroup(cur_group())),
      AFD = pct(sum(AfD) / bundSumByGroup(cur_group())),
      Sonstige = pct(sum(Sonstige) / bundSumByGroup(cur_group()))
    )
  return(res)
}