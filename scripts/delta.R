delta <- cases %>%
  filter(NeuerFall %in% c(1,-1)) %>%
  group_by(Meldedatum, Art = NeuerFall) %>%
  summarize(Fallzahl = sum(AnzahlFall)) %>%
  as.data.frame() %>%
  mutate(Art = c("Storno", "-", "Neu")[Art + 2]) %>%
  mutate(Inzidenzrelevant = c("Nein", "Ja")[((today() - Meldedatum <= 7)) + 1])
delta <- if (!exists("delta")) "Keine neuen Fälle oder Nachmeldungen" else delta

delta.dem <- cases %>%
  filter(NeuerFall %in% c(1,-1)) %>%
  group_by(Meldedatum, Art = NeuerFall, Altersgruppe) %>%
  summarize(Fallzahl = sum(AnzahlFall)) %>%
  as.data.frame() %>%
  mutate(Art = c("Storno", "-", "Neu")[Art + 2]) %>%
  mutate(Inzidenzrelevant = c("Nein", "Ja")[((today() - Meldedatum <= 7)) + 1]) %>%
  pivot_wider(names_from = "Altersgruppe", values_from = "Fallzahl") %>%
  select(sort(names(.))) %>%
  as.data.frame()
delta.dem <- if (!exists("delta.dem")) "Keine neuen Fälle oder Nachmeldungen" else delta.dem


delta.out <- merge(delta,delta.dem)
delta.out[is.na(delta.out)] <- 0