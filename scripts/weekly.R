weekly <- timeline %>%
  mutate(across(3:4, round, 1)) %>%
  as.data.frame() %>%
  tail(7) %>%
  mutate(Inzidenz.1t = round(AnzahlFall / EWO * 100000, 1)) %>%
  rename(I.1d = 5) %>%
  mutate(cumsum(AnzahlFall)) %>%
  rename(lfd.sum = 6) %>%
  left_join(delta) %>%
  mutate(Fallzahl = replace_na(Fallzahl, 0)) %>%
  mutate(NeuInzidenz = round(Fallzahl / EWO * 100000, 1))

weekly.plot <- weekly %>%
  ggplot(aes(x = Meldedatum, y = Inzidenz)) +
  geom_bar(stat = "identity", width = 1, fill = "grey") +
  geom_bar(aes(y = I.1d, x = Meldedatum), stat = "identity") +
  geom_bar(aes(y = NeuInzidenz, x = Meldedatum), stat = "identity", fill = "red") +
  geom_text(aes(label = paste0("", round(Inzidenz, 1))), position = position_dodge(width = 0.9), vjust = -2, size = 4) +
  geom_text(aes(y = -4, label = paste0(round(AnzahlFall, 1))), position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = Schwellen[2, 1], linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = Schwellen[1, 1], linetype = "dashed", color = "green", size = 1) +
  scale_x_date(date_breaks = "1 day", date_labels = "%a, %d.%m") +
  labs(title = paste0(
    "Neu gemeldete Fälle in den letzten 7 Tagen: ",
    sum(weekly$AnzahlFall)
  ))
