timeline <- cases %>%
  filter(NeuerFall %in% c(1,0)) %>%
  group_by(Meldedatum) %>%
  arrange(Meldedatum) %>%
  summarise_at(c("AnzahlFall"), sum, na.rm = TRUE) %>%
  complete(Meldedatum = seq.Date(min(Meldedatum), today() - 1, by = "day"))

timeline$AnzahlFall <- replace(timeline$AnzahlFall, is.na(timeline$AnzahlFall), 0)

# Rückblickende Summe der Fallzahlen der letzten 7 Tage, von jedem einzelnen
# Datum aus gerechnet, das ganze auf 100.000 Einwohner normalisiert ergibt
# die berüchtigte "7-Tage-je-100.000-Einwohner-Inzidenz"
timeline$Inzidenz <- round(rollsum(timeline$AnzahlFall, 7,
                             fill = 0,
                             align = "right") * 100000 / EWO,
                           1) 

# Höchste Inzidenz der jeweils vorausgehenden 7 Tage, liefert die Aussage, ob
# ein bestimmter Wert in dieser Zeit nicht überschritten wurde
timeline$Inzidenz.max7 <- rollmax(timeline$Inzidenz, 7,
                                  fill = 0,
                                  align = "right"
)


# In development:

# fig.timeline.cases <- timeline %>%
#   plot_ly(x = ~Meldedatum, y = ~AnzahlFall, type = "scatter", mode = "lines")
# 
# fig.timeline.Inz <- timeline %>%
#   plot_ly(x = ~Meldedatum, y = ~Inzidenz, type = "scatter", mode = "lines")
# 
# fig.timeline.Inz
# 
# 
# dash.09262 <- dash.cases.pivot %>% filter(AGS == "09262")
# fig.timeline.cases %>% add_lines(x = ~dash.09262$Datum, y = ~dash.09262$Fallzahl)
