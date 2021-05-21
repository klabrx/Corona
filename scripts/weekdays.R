weekdays <- timeline %>%
  mutate(WD = weekdays(Meldedatum)) %>%
  mutate(DoW = wday(Meldedatum)) %>%
  filter(Meldedatum > max(Meldedatum - backweek * 7)) %>%
  group_by(DoW, WD) %>%
  summarize(Fallzahl_Durchschnitt = round(mean(AnzahlFall), 2), sd = sd(AnzahlFall), .groups = "keep") %>%
  arrange(DoW) %>%
  as.data.frame() %>%
  select(Wochentag = WD, Fallzahl_Durchschnitt, DoW)