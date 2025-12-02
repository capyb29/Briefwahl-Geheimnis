library(tidyverse)
library(sf)
library(stringdist)
source("main.r")


# Anteil der Briefwähler von allen Wahlberechtigten
wahlkreise = st_read("../data/wahlkreise/wahlkreise.shp")

#Betiligung nach Art in Kreis

betArtKreis = kreis_daten_gesamt %>% group_by(Jahr, `Wahlkreis-Nr.`, Wahlbezirksart) %>% summarise(Wähler = sum(Gültige))
ichhörediestimme = kreis_daten_gesamt %>% group_by(Jahr, `Wahlkreis-Nr.`) %>% summarise(Stimmen = sum(Gültige))
betArtKreis = betArtKreis %>% left_join(ichhörediestimme, by = c("Jahr", "Wahlkreis-Nr."))
betArtKreis$artBeteiligung = pct(betArtKreis$Wähler / betArtKreis$Stimmen)

plotBetArt = function(df) {
  df = left_join(wahlkreise, df, by = c("WKR_NR" = "Wahlkreis-Nr."))
  ggplot(df) +
    geom_sf(aes(fill = artBeteiligung)) +
    scale_fill_gradient(
      low = "red",
      high = "green",
      limits = c(10 + (unique(
        df$Wahlbezirksart == "Urne"
      ) * 20), 70 + (unique(
        df$Wahlbezirksart == "Urne"
      ) * 20))
    ) +
    labs(
      title = paste0("Bundestagswahl ", unique(df$Jahr)),
      subtitle = paste("Wahlbezirksart:", unique(df$Wahlbezirksart)),
      fill = "Beteiligung (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(
        hjust = 0.5,
        size = 12,
        face = "bold"
      ),
      legend.text = element_text(size = 8)
    )
}

plotBetArt(betArtKreis[betArtKreis$Jahr == 2017 &
                         betArtKreis$Wahlbezirksart == "Brief", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2021 &
                         betArtKreis$Wahlbezirksart == "Brief", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2025 &
                         betArtKreis$Wahlbezirksart == "Brief", ])

plotBetArt(betArtKreis[betArtKreis$Jahr == 2017 &
                         betArtKreis$Wahlbezirksart == "Urne", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2021 &
                         betArtKreis$Wahlbezirksart == "Urne", ])
plotBetArt(betArtKreis[betArtKreis$Jahr == 2025 &
                         betArtKreis$Wahlbezirksart == "Urne", ])

artAnteilJahre = function() {
  resBundAnalyse = bundAnalyse(group = "Bezirksart")
  
  x1 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Brief"]
  y1 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Brief"]
  
  x2 = resBundAnalyse$Jahr[resBundAnalyse$Bezirksart == "Urne"]
  y2 = resBundAnalyse$Bezirksart_Anteil[resBundAnalyse$Bezirksart == "Urne"]
  
  plot(
    x1,
    y1,
    type = "b",
    frame = FALSE,
    pch = 19,
    col = "red",
    xlab = "Jahr",
    ylab = "Anteil in %",
    xlim = c(2016, 2026),
    ylim = c(0, 100),
    main = "Anteil der Brief- und Urnenwahl pro Jahr"
  )
  
  lines(
    x2,
    y2,
    pch = 19,
    col = "blue",
    type = "b",
    lty = 1
  )
  abline(y2[1], 0, col = "blue", lty = 2)
  abline(y1[1], 0, col = "red", lty = 2)
  
  text(2016.6,
       33,
       paste(y1[1], "%"),
       col = "red",
       cex = 0.7)
  text(2016.6,
       75.8,
       paste(y2[1], "%"),
       col = "blue",
       cex = 0.7)
  
  legend(
    2016,
    103,
    legend = c("Brief", "Urne"),
    col = c("red", "blue"),
    lty = 1,
    pch = 19,
    cex = 0.9
  )
}

artAnteilJahre()

plot_wahlbeteiligung_jahr = function(df, jahr) {
  df_jahr = df %>% filter(Jahr == jahr) %>%
    mutate(ba = (Wahlbezirksart == "Brief") * Anteil) %>%
    arrange(desc(ba))
  
  
  m = kreis_daten_gesamt[kreis_daten_gesamt$Jahr == jahr, ] %>% group_by(Wahlbezirksart) %>%
    summarise(Anteil = pct(sum(Wähler) / sum(kreis_daten_gesamt$Wähler[kreis_daten_gesamt$Jahr == jahr])))
  m = m$Anteil[1]
  print(m)
  
  # Plot erstellen mit gestapelten Balken für Urne/Brief je Bundesland
  p = ggplot(df_jahr, aes(x = Land, y = Anteil, fill = Wahlbezirksart)) +
    geom_bar(stat = "identity",
             position = "stack",
             width = 0.8) +
    labs(
      title = paste("Anteil Brief- und Urnenwahl in", jahr),
      x = "Bundesland",
      y = "Anteil (%)",
      fill = "Wahlbezirksart"
    ) +
    geom_text(
      aes(label = round(Anteil)),
      position = position_stack(vjust = 0.5),
      size = 2.7,
      color = "white"
    ) +
    geom_hline(
      yintercept = m,
      color = "blue",
      linetype = "dashed",
      size = 1
    ) +
    annotate(
      "text",
      x = length(levels(df_jahr$Land)) / 2 - 0.5,
      y = m + 7,
      label = paste(m, "%"),
      size = 5,
      hjust = 0,
      fontface = "bold",
      color = "blue"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(
        hjust = 0.5,
        size = 12,
        face = "bold"
      ),
      legend.text = element_text(size = 8)
    )
  
  print(p)
}
anteileLänderJahre = kreis_daten_gesamt %>% group_by(Jahr, Land, Wahlbezirksart) %>%
  summarise(Anteil = pct(sum(Wähler) / sum(kreis_daten_gesamt[kreis_daten_gesamt$Jahr == Jahr &
                                                                kreis_daten_gesamt$Land == Land, "Wähler"])))

# Bayern und Rheinland-Pfalz immer oben mit dabei
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2017)
ggsave("./plots/ArtAnteileLänder17.png")
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2021)
ggsave("./plots/ArtAnteileLänder21.png")
plot_wahlbeteiligung_jahr(anteileLänderJahre, 2025)
ggsave("./plots/ArtAnteileLänder25.png")


# Plot Geschlecht Art Partei
bundArtGeschlecht = bundAnalyse(group = c("Bezirksart", "Geschlecht"))
parties = c("CDU_CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AFD", "Sonstige")
df_long = bundArtGeschlecht %>% pivot_longer(cols = all_of(parties),
                                             names_to = "Partei",
                                             values_to = "Stimmenanteil")

df_long <- df_long %>%
  group_by(Jahr, Bezirksart, Geschlecht) %>%
  mutate(Partei = factor(Partei, levels = Partei[order(-Stimmenanteil)])) %>%
  ungroup()

ggplot(df_long) +
  aes(x = Partei, y = Stimmenanteil, fill = Geschlecht) +
  labs(title = "Wahlanteile nach Bezirksart und Geschlecht über die Jahre") +
  geom_col(position = position_dodge()) +
  facet_grid(Jahr ~ Bezirksart) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.text = element_text(size = 8)
  )
ggsave("./plots/ParteiArtGeschlechtJahre.png")

# vorläufig
# Plot with Brief and Urne side by side, stacked by Geschlecht
ggplot(bundArtGeschlecht,
       aes(x = factor(Jahr), y = Bezirksart_Anteil, fill = Geschlecht)) +
  geom_bar(aes(group = Bezirksart),
           stat = "identity",
           position = position_stack()) +
  facet_wrap(~ Bezirksart) +
  labs(x = "Year",
       y = "Bezirksart Anteil",
       fill = "Geschlecht",
       title = "Bezirksart Anteile nach Geschlecht über die Jahre") +
  theme_minimal() +
  scale_x_discrete(expand = expansion(add = c(0.2, 0.2))) +
  theme(legend.position = "top")

# hypothesentest ob wähler von geschlecht abhängt,
tBrief = t.test(Summe ~ Geschlecht, data = bund_komplett[bund_komplett$Bezirksart == "Brief", ])
tUrne = t.test(Summe ~ Geschlecht, data = bund_komplett[bund_komplett$Bezirksart == "Urne", ])

tTestRes = data.frame(Bezirksart = c("Brief", "Urne"),
                      `p-value` = c(tBrief, tUrne))

# altersgruppen
gruppen = bund_komplett %>% group_by(Jahr, Bezirksart, Geburtsjahresgruppe) %>% summarise(
  Wähler = sum(Summe),
  CDU_CSU = sum(CDU) + sum(CSU),
  SPD = sum(SPD),
  GRÜNE = sum(GRÜNE),
  LINKE = sum(`DIE LINKE`),
  AFD = sum(AfD),
  FDP = sum(FDP)
)

gruppen17 = gruppen[gruppen$Jahr == 2017, ] %>% pivot_longer(cols = CDU_CSU:FDP,
                                                             names_to = "Partei",
                                                             values_to = "Stimmen")
gruppen21 = gruppen[gruppen$Jahr == 2021, ] %>% pivot_longer(cols = CDU_CSU:FDP,
                                                             names_to = "Partei",
                                                             values_to = "Stimmen")
gruppen25 = gruppen[gruppen$Jahr == 2025, ] %>% pivot_longer(cols = CDU_CSU:FDP,
                                                             names_to = "Partei",
                                                             values_to = "Stimmen")

ggplot(gruppen25) +                                                                                 ##
  aes(x = Geburtsjahresgruppe, y = Stimmen, fill = Partei) +                                        ##
  geom_bar(stat = "identity", position = "dodge") +                                                 ##
  scale_fill_manual(values = c(
    "#382BF0",
    "#17171C",
    "#FFE419",
    "#24C210",
    "#D92DC5",
    "#D92E2E"
  )) + ##
  facet_wrap( ~ Bezirksart) +                                                                         ##
  labs(
    x = "Geburtsjahresgruppe",
    y = "Anzahl Stimmen",
    fill = "Partei",
    ##
    title = "Stimmenanzahl nach Geburtsjahresgruppe, Partei und Bezirksart (2017)"
  ) +            ##
  theme_minimal() +                                                                                 ##
  theme(axis.text.x = element_text(angle = 45, hjust = 1))                                          ##
