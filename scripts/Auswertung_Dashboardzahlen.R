

# Abruf der eigentlichen Daten
dash.inz <- read_excel(destfile, sheet=7, skip=4, col_names = FALSE) %>% select(-1) %>% t()  %>% data.frame()

# AdHoc-Funktion für die Umwandlung der bizarren RKI-Datumsangaben
as.excel.date=function(x) as.Date(x,origin='1899-12-30')

# Datum teilweise als Text im Format "TT.MM.JJJJ", teilweise als Charset im 
# Format "#####.00000" angegeben, muss getrennt geparst werden.
# Zielformat: "JJJJ-MM-DD" als date
dash.inz[which(nchar(dash.inz$X1) == 10),]$X1 <- as.Date(parse_date_time(dash.inz[which(nchar(dash.inz$X1) == 10),]$X1, c('d.m.y')))
dash.inz[which(nchar(dash.inz$X1) == 11),]$X1 <- dash.inz[which(nchar(dash.inz$X1) == 11),]$X1 %>% as.numeric() %>% as.excel.date()
# vorübergehendes Zusammenfassen von AGS und Kreisname
colnames(dash.inz) <- paste0(str_pad(dash.inz[2,],5,side=c("left"), pad="0"),"",dash.inz[1,])

# Entsorgen der ersten beiden Zeilen
dash.inz <- dash.inz[-(1:2),]


dash.inz$Datum <- as.Date(as.numeric(dash.inz$`0LKNRLK`))


#Umwandlung breit in lang, Trennung von AGS und Kreis
dash.inz.pivot <- dash.inz %>%
  select(-1) %>%
  pivot_longer(!Datum, 
               names_to = "Kreis",
               values_to = "Inzidenz") %>% 
  mutate(Inzidenz = round(as.numeric(Inzidenz),1)) %>%
  separate(Kreis,c("AGS","Kreis"), sep=5)

dash.inz.pivot$mysize <- rep(0.5, nrow(dash.inz.pivot))
dash.inz.pivot$mysize[dash.inz.pivot$AGS=="09262"] <- 1

dash.inz.plot <- dash.inz.pivot %>% filter(Kreis %in% c("SK Passau",
                                                        "LK Passau",
                                                        "SK Landshut",
                                                        "LK Freyung-Grafenau",
                                                        "LK Rottal-Inn",
                                                        "LK Deggendorf")) %>%
  ggplot(aes(x = Datum, y = Inzidenz, group=Kreis, color=Kreis, size=mysize)) + 
  geom_line() +
  scale_size(range = c(0.5, 1), guide="none")



# ... und nochmal das gleiche, nur diesmal für die Fallzahlen
dash.cases <- read_excel(destfile, sheet=6, skip=4, col_names = FALSE) %>% select(-1) %>% t()  %>% data.frame()

# AdHoc-Funktion für die Umwandlung der bizarren RKI-Datumsangaben
as.excel.date=function(x) as.Date(x,origin='1899-12-30')

# Datum teilweise als Text im Format "TT.MM.JJJJ", teilweise als Charset im 
# Format "#####.00000" angegeben, muss getrennt geparst werden.
# Zielformat: "JJJJ-MM-DD" als date
dash.cases[which(nchar(dash.cases$X1) == 10),]$X1 <- as.Date(parse_date_time(dash.cases[which(nchar(dash.cases$X1) == 10),]$X1, c('d.m.y')))
dash.cases[which(nchar(dash.cases$X1) == 11),]$X1 <- dash.cases[which(nchar(dash.cases$X1) == 11),]$X1 %>% as.numeric() %>% as.excel.date()
# vorübergehendes Zusammenfassen von AGS und Kreisname
colnames(dash.cases) <- paste0(str_pad(dash.cases[2,],5,side=c("left"), pad="0"),"",dash.cases[1,])

# Entsorgen der ersten beiden Zeilen
dash.cases <- dash.cases[-(1:2),]


dash.cases$Datum <- as.Date(as.numeric(dash.cases$`0LKNRLK`))


#Umwandlung breit in lang, Trennung von AGS und Kreis
dash.cases.pivot <- dash.cases %>%
  select(-1) %>%
  pivot_longer(!Datum, 
               names_to = "Kreis",
               values_to = "Fallzahl") %>% 
  separate(Kreis,c("AGS","Kreis"), sep=5)

dash.cases.pivot$mysize <- rep(0.5, nrow(dash.cases.pivot))
dash.cases.pivot$mysize[dash.cases.pivot$AGS=="09262"] <- 1

dash.cases.plot <- dash.cases.pivot %>% filter(Kreis %in% c("SK Passau",
                                                            "LK Passau",
                                                            "SK Landshut",
                                                            "LK Freyung-Grafenau",
                                                            "LK Rottal-Inn",
                                                            "LK Deggendorf")) %>%
  ggplot(aes(x = Datum, y = Inzidenz, group=Kreis, color=Kreis, size=mysize)) + 
  geom_line() +
  scale_size(range = c(0.5, 1), guide="none")
