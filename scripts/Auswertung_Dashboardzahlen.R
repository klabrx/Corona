# Abruf der eigentlichen Daten:
dash.inz <- read_excel(destfile, # vorgeladene Exceldatei
  sheet = "LK_7-Tage-Inzidenz (fixiert)",
  skip = 4, # ohne die ersten 4 Zeilen
  col_names = FALSE
) %>%
  select(-1) %>% # ohne die erste Spalte
  t() %>% # Datum zeilenweise, Kreis spaltenweise
  as_tibble()

Datumsbereich <- seq(as.Date(parse_date_time(dash.inz[3, 1], c("d.m.y"))), by = "day", length = nrow(dash.inz) - 2)

colnames(dash.inz) <- paste0(str_pad(dash.inz[2, ], 5, side = c("left"), pad = "0"), "", dash.inz[1, ])

# Entsorgen der ersten beiden Zeilen
dash.inz <- dash.inz[-(1:2), ] %>% mutate(Datum = Datumsbereich)


# dash.inz$Datum <- as.Date(as.numeric(dash.inz$`0LKNRLK`))


# Umwandlung breit in lang, Trennung von AGS und Kreis
dash.inz.pivot <- dash.inz %>%
  select(-1) %>%
  pivot_longer(!Datum,
    names_to = "Kreis",
    values_to = "Inzidenz"
  ) %>%
  mutate(Inzidenz = round(as.numeric(Inzidenz), 1)) %>%
  separate(Kreis, c("AGS", "Kreis"), sep = 5)

dash.inz.pivot$mysize <- rep(0.5, nrow(dash.inz.pivot))
dash.inz.pivot$mysize[dash.inz.pivot$AGS == "09262"] <- 1

dash.inz.plot <- dash.inz.pivot %>%
  filter(Kreis %in% c(
    "SK Passau",
    "LK Passau",
    "SK Landshut",
    "LK Freyung-Grafenau",
    "LK Rottal-Inn",
    "LK Deggendorf"
  )) %>%
  ggplot(aes(x = Datum, y = Inzidenz, group = Kreis, color = Kreis, size = mysize)) +
  geom_line() +
  scale_size(range = c(0.5, 1), guide = "none")



# Abruf der eigentlichen Daten:
dash.cases <- read_excel(destfile, # vorgeladene Exceldatei
  sheet = "LK_7-Tage-Fallzahlen (fixiert)",
  skip = 4, # ohne die ersten 4 Zeilen
  col_names = FALSE
) %>%
  select(-1) %>% # ohne die erste Spalte
  t() %>% # Datum zeilenweise, Kreis spaltenweise
  as_tibble()


colnames(dash.cases) <- paste0(str_pad(dash.cases[2, ], 5, side = c("left"), pad = "0"), "", dash.cases[1, ])

# Entsorgen der ersten beiden Zeilen
dash.cases <- dash.cases[-(1:2), ] %>% mutate(Datum = Datumsbereich)


# dash.cases$Datum <- as.Date(as.numeric(dash.cases$`0LKNRLK`))


# Umwandlung breit in lang, Trennung von AGS und Kreis
dash.cases.pivot <- dash.cases %>%
  select(-1) %>%
  pivot_longer(!Datum,
    names_to = "Kreis",
    values_to = "Fallzahl"
  ) %>%
  separate(Kreis, c("AGS", "Kreis"), sep = 5)

dash.cases.pivot$mysize <- rep(0.5, nrow(dash.cases.pivot))
dash.cases.pivot$mysize[dash.cases.pivot$AGS == "09262"] <- 1



dash.cases.AGS <- dash.cases.pivot %>% filter(AGS == AG)
dash.inz.AGS <- dash.inz.pivot %>% filter(AGS == AG)
dash.AGS <- dash.cases.AGS %>% full_join(dash.inz.AGS)

if (tail(dash.AGS$Datum, 1) < Sys.Date()) {
  dash.AGS <- dash.AGS %>%
    add_row(
      Datum = as.Date(Sys.time()),
      AGS = "09262",
      Kreis = "SK Passau",
      Fallzahl = as.character(sum(tail(timeline, 7)$AnzahlFall)),
      mysize = 1,
      Inzidenz = tail(timeline, 1)$Inzidenz
    )
}



dash.cases.plot <- dash.cases.pivot %>%
  filter(Kreis %in% c(
    "SK Passau",
    "LK Passau",
    "SK Landshut",
    "LK Freyung-Grafenau",
    "LK Rottal-Inn",
    "LK Deggendorf"
  )) %>%
  ggplot(aes(x = Datum, y = Inzidenz, group = Kreis, color = Kreis, size = mysize)) +
  geom_line() +
  scale_size(range = c(0.5, 1), guide = "none")



# Vergleich dashboard vs. timeline

comparison <- timeline %>%
  mutate(Datum = Meldedatum + 1) %>%
  inner_join(dash.AGS, by = "Datum") %>%
  select(Meldedatum, Inz.API = Inzidenz.x, Inz.Dashboard = Inzidenz.y) %>%
  pivot_longer(!Meldedatum, names_to = "Quelle", values_to = "Inzidenz") %>%
  plot_ly(x = ~Meldedatum, y = ~Inzidenz, type = "scatter", color = ~Quelle, mode = "lines")
# comparison

# Formel für Zeitdauer
# max(dash.AGS[which(dash.AGS$Inzidenz > 100),]$Datum) - max(dash.AGS[which(dash.AGS$Inzidenz < 100),]$Datum)
# max(-6,as.numeric(max(dash.AGS[which(dash.AGS$Inzidenz > 100),]$Datum) - max(dash.AGS[which(dash.AGS$Inzidenz < 100),]$Datum)))

Dauer.Grenzwert <- function(Grenzwert, low = -10, high = 10) {
  Tage <- max(dash.AGS[which(dash.AGS$Inzidenz > Grenzwert), ]$Datum) - max(dash.AGS[which(dash.AGS$Inzidenz < Grenzwert), ]$Datum)
  Tage <- max(low, min(high, as.numeric(Tage)))
  return(Tage)
}

Statustext <- function(Tage) {
  return(cut(Tage,
    breaks = c(-Inf, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, Inf),
    labels = c(
      "Seit mehr als 5 Tagen unterschritten",
      "seit 5 Tagen unterschritten: Heute Bekanntmachung!",
      "seit 4 Tagen unterschritten: Morgen Bekanntmachung!",
      "seit 3 Tagen unterschritten: Übermorgen Bekanntmachung!",
      "seit 2 Tagen unterschritten",
      "erstmals unterschritten",
      "aktueller Grenzwert eingehalten",
      "erstmals überschritten: Übermorgen Bekanntmachung!",
      "seit zwei Tagen überschritten: Morgen Bekanntmachung!",
      "seit drei Tagen überschritten: Heute Bekanntmachung!",
      "seit mehr als 3 Tagen überschritten"
    )
  ))
}

Notbrems.Grenzwerte <- limits %>%
  bind_cols(sapply(limits, Dauer.Grenzwert)) %>%
  rename(Grenzwert = 1, Tage = 2) %>%
  mutate(Status = Statustext(Tage)) %>%
  as_tibble()

Notbremse <- max(-6, min(4, Dauer.Grenzwert(100)))

Notbremse

f <- list(
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Über-/Unterschreitung seit Tagen",
  titlefont = f
)
y <- list(
  title = "Grenzwert",
  titlefont = f
)
fig.Notbremse <- Notbrems.Grenzwerte %>%
  plot_ly(x = ~Tage, y = ~ str_pad(as.character(Grenzwert), 3, "left", "0"), type = "bar", orientation = "h")

# fig.Notbremse %>% layout(xaxis = x, yaxis = y)
