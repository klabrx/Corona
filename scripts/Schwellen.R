Schwellen <- params$limits %>%
  as.data.frame() %>%
  rename(Grenzwert = 1) %>%
  mutate(bei_Fallzahl = floor(Grenzwert * EWO/100000)) 
Schwellen$ok_seit <- sapply(Schwellen$Grenzwert, ok)
# Schwellen$ok_RKI <- sapply(Schwellen$Grenzwert, checkdash)

names(Schwellen) <- c("Grenzwert","max. Fälle","Tag(e)")