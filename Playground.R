
estimate <- timeline %>%
  mutate(WD = weekdays(Meldedatum)) %>%
  mutate(DoW = wday(Meldedatum)) %>%
  filter(Meldedatum > max(Meldedatum-backweek*7)) %>%
  group_by(DoW, WD) %>%
  summarize(Fallzahl_Durchschnitt = round(mean(AnzahlFall),2), sd=sd(AnzahlFall),.groups="keep") %>%
  arrange(DoW) %>%
  as.data.frame() %>%
  select(Wochentag=WD, Fallzahl_Durchschnitt, DoW)
estimate

last.week <- weekly



two.week <- weekly %>% 
  complete(Meldedatum=seq.Date(min(Meldedatum), 
                               max(Meldedatum) + 7,
                               by="day"))
two.week[8:14,2] <- -two.week[1:7,2]

prognosis <- two.week %>%
  mutate(Anzahl.Fall.ext = ifelse(is.na(AnzahlFall), -AnzahlFall, AnzahlFall), DoW=wday(Meldedatum)) %>%
  mutate(lfd.sum = cumsum(AnzahlFall)) %>%
  full_join(weekdays,by="DoW") %>%
  tail(8) %>%
  select(Inzidenz,Meldedatum,AnzahlFall,Fallzahl_Durchschnitt) %>%
  mutate(var0 = AnzahlFall,
         var1 = (AnzahlFall+Fallzahl_Durchschnitt),
         var2 = (AnzahlFall+2*Fallzahl_Durchschnitt))
prognosis[1,4:7] <- two.week[7,6]
prognosis <- prognosis %>%
  select(Meldedatum,var0,var1,var2) %>%
  mutate(sum0 = cumsum(var0),
         sum1 = cumsum(var1),
         sum2 = cumsum(var2)) %>%
  mutate(Inz0 = sum0/EWO*100000,
         Inz1 = sum1/EWO*100000,
         Inz2 = sum2/EWO*100000) %>%
  ggplot(aes(x=Meldedatum)) +
  geom_crossbar(aes(y=Inz1, ymin = Inz0, ymax = Inz2)) +
  geom_hline(yintercept=Schwellen[3,1], linetype="dashed", color = "red", size=1) +
  geom_hline(yintercept=Schwellen[2,1], linetype="dashed", color = "yellow", size=1) +
  geom_hline(yintercept=Schwellen[1,1], linetype="dashed", color = "green", size=1) +
  scale_x_date(date_breaks = "1 day", date_labels = "%a, %d.%m.%Y") 

prognosis









axlab <- as.data.frame(c("DT","DT+1","DT+2","DT+3","DT+4","DT+5","DT+6","DT+7"),col.names=c("x.lab")) %>% transmute(x.lab=1)

load("./data/AKL.RData")

Altersstruktur <- cases %>%
  filter(NeuerFall %in% c(0,1)) %>%
  filter(Meldedatum > Sys.Date()-8) %>%
  group_by(Altersklasse=Altersgruppe) %>%
  summarise(Fälle = sum(AnzahlFall)) %>%
  merge(Altersklassen, all.y=TRUE) %>%
  mutate(Kohorteninzidenz = round(Fälle/EWO*100000,1)) %>%
  select(-3)  %>%
  t() %>%
  as.data.frame()

names(Altersstruktur) <- Altersstruktur[1,]

Altersstruktur <- Altersstruktur[2:3,]
print(Altersstruktur, row.names = TRUE, col.names = FALSE)




str(timeline)
attach(timeline)
Inzidenzen <- timeline[rev(order(Meldedatum)),3]
detach(timeline)

for(limit in Schwellen$Grenzwert) print(Schwellen$Grenzwert)

Schwellen$ok_35 <- as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= 35),])$Meldedatum)))
Schwellen$ok_50 <- as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= 50),])$Meldedatum)))
Schwellen$ok_100 <- as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= 100),])$Meldedatum)))
Schwellen$ok_165 <- as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= 165),])$Meldedatum)))
ok
=======
  estimate <- timeline %>%
  mutate(WD = weekdays(Meldedatum)) %>%
  mutate(DoW = wday(Meldedatum)) %>%
  filter(Meldedatum > max(Meldedatum-backweek*7)) %>%
  group_by(DoW, WD) %>%
  summarize(Fallzahl_Durchschnitt = round(mean(AnzahlFall),2), sd=sd(AnzahlFall),.groups="keep") %>%
  arrange(DoW) %>%
  as.data.frame() %>%
  select(Wochentag=WD, Fallzahl_Durchschnitt, DoW)
estimate

last.week <- weekly



two.week <- weekly %>% 
  complete(Meldedatum=seq.Date(min(Meldedatum), 
                               max(Meldedatum) + 7,
                               by="day"))
two.week[8:14,2] <- -two.week[1:7,2]

prognosis <- two.week %>%
  mutate(Anzahl.Fall.ext = ifelse(is.na(AnzahlFall), -AnzahlFall, AnzahlFall), DoW=wday(Meldedatum)) %>%
  mutate(lfd.sum = cumsum(AnzahlFall)) %>%
  full_join(weekdays,by="DoW") %>%
  tail(8) %>%
  select(Inzidenz,Meldedatum,AnzahlFall,Fallzahl_Durchschnitt) %>%
  mutate(var0 = AnzahlFall,
         var1 = (AnzahlFall+Fallzahl_Durchschnitt),
         var2 = (AnzahlFall+2*Fallzahl_Durchschnitt))
prognosis[1,4:7] <- two.week[7,6]
prognosis <- prognosis %>%
  select(Meldedatum,var0,var1,var2) %>%
  mutate(sum0 = cumsum(var0),
         sum1 = cumsum(var1),
         sum2 = cumsum(var2)) %>%
  mutate(Inz0 = sum0/EWO*100000,
         Inz1 = sum1/EWO*100000,
         Inz2 = sum2/EWO*100000) %>%
  ggplot(aes(x=Meldedatum)) +
  geom_crossbar(aes(y=Inz1, ymin = Inz0, ymax = Inz2)) +
  geom_hline(yintercept=Schwellen[3,1], linetype="dashed", color = "red", size=1) +
  geom_hline(yintercept=Schwellen[2,1], linetype="dashed", color = "yellow", size=1) +
  geom_hline(yintercept=Schwellen[1,1], linetype="dashed", color = "green", size=1) +
  scale_x_date(date_breaks = "1 day", date_labels = "%a, %d.%m.%Y") 

prognosis









axlab <- as.data.frame(c("DT","DT+1","DT+2","DT+3","DT+4","DT+5","DT+6","DT+7"),col.names=c("x.lab")) %>% transmute(x.lab=1)

load("./data/AKL.RData")

Altersstruktur <- cases %>%
  filter(NeuerFall %in% c(0,1)) %>%
  filter(Meldedatum > Sys.Date()-8) %>%
  group_by(Altersklasse=Altersgruppe) %>%
  summarise(Fälle = sum(AnzahlFall)) %>%
  merge(Altersklassen, all.y=TRUE) %>%
  mutate(Kohorteninzidenz = round(Fälle/EWO*100000,1)) %>%
  select(-3)  %>%
  t() %>%
  as.data.frame()

names(Altersstruktur) <- Altersstruktur[1,]

Altersstruktur <- Altersstruktur[2:3,]
print(Altersstruktur, row.names = TRUE, col.names = FALSE)


tmp <- tail(timeline, 10)
tmp[which (tmp$Meldedatum == "2021-04-27"),2]



as.numeric(max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= 100),])$Meldedatum))



=======
  estimate <- timeline %>%
  mutate(WD = weekdays(Meldedatum)) %>%
  mutate(DoW = wday(Meldedatum)) %>%
  filter(Meldedatum > max(Meldedatum-backweek*7)) %>%
  group_by(DoW, WD) %>%
  summarize(Fallzahl_Durchschnitt = round(mean(AnzahlFall),2), sd=sd(AnzahlFall),.groups="keep") %>%
  arrange(DoW) %>%
  as.data.frame() %>%
  select(Wochentag=WD, Fallzahl_Durchschnitt, DoW)
estimate

last.week <- weekly



two.week <- weekly %>% 
  complete(Meldedatum=seq.Date(min(Meldedatum), 
                               max(Meldedatum) + 7,
                               by="day"))
two.week[8:14,2] <- -two.week[1:7,2]

prognosis <- two.week %>%
  mutate(Anzahl.Fall.ext = ifelse(is.na(AnzahlFall), -AnzahlFall, AnzahlFall), DoW=wday(Meldedatum)) %>%
  mutate(lfd.sum = cumsum(AnzahlFall)) %>%
  full_join(weekdays,by="DoW") %>%
  tail(8) %>%
  select(Inzidenz,Meldedatum,AnzahlFall,Fallzahl_Durchschnitt) %>%
  mutate(var0 = AnzahlFall,
         var1 = (AnzahlFall+Fallzahl_Durchschnitt),
         var2 = (AnzahlFall+2*Fallzahl_Durchschnitt))
prognosis[1,4:7] <- two.week[7,6]
prognosis <- prognosis %>%
  select(Meldedatum,var0,var1,var2) %>%
  mutate(sum0 = cumsum(var0),
         sum1 = cumsum(var1),
         sum2 = cumsum(var2)) %>%
  mutate(Inz0 = sum0/EWO*100000,
         Inz1 = sum1/EWO*100000,
         Inz2 = sum2/EWO*100000) %>%
  ggplot(aes(x=Meldedatum)) +
  geom_crossbar(aes(y=Inz1, ymin = Inz0, ymax = Inz2)) +
  geom_hline(yintercept=Schwellen[3,1], linetype="dashed", color = "red", size=1) +
  geom_hline(yintercept=Schwellen[2,1], linetype="dashed", color = "yellow", size=1) +
  geom_hline(yintercept=Schwellen[1,1], linetype="dashed", color = "green", size=1) +
  scale_x_date(date_breaks = "1 day", date_labels = "%a, %d.%m.%Y") 

prognosis









axlab <- as.data.frame(c("DT","DT+1","DT+2","DT+3","DT+4","DT+5","DT+6","DT+7"),col.names=c("x.lab")) %>% transmute(x.lab=1)

load("./data/AKL.RData")

Altersstruktur <- cases %>%
  filter(NeuerFall %in% c(0,1)) %>%
  filter(Meldedatum > Sys.Date()-8) %>%
  group_by(Altersklasse=Altersgruppe) %>%
  summarise(Fälle = sum(AnzahlFall)) %>%
  merge(Altersklassen, all.y=TRUE) %>%
  mutate(Kohorteninzidenz = round(Fälle/EWO*100000,1)) %>%
  select(-3)  %>%
  t() %>%
  as.data.frame()

names(Altersstruktur) <- Altersstruktur[1,]

Altersstruktur <- Altersstruktur[2:3,]
print(Altersstruktur, row.names = TRUE, col.names = FALSE)




str(timeline)
attach(timeline)
Inzidenzen <- timeline[rev(order(Meldedatum)),3]
detach(timeline)

for(limit in Schwellen$Grenzwert) print(Schwellen$Grenzwert)
attach(timeline)
Schwellen$OK <- as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= Grenzwert)])$Meldedatum)))

detach(timeline)

as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(timeline$Inzidenz >= 35),])$Meldedatum)))
ok <- function(Grenzwert){
  result <- as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= Grenzwert),])$Meldedatum)))
  if(result == 0) return("Nicht unterschritten") else
    if(result == 1) return("seit 1 Tag unterschritten") else
      return(paste0("seit ",result," Tagen unterschritten"))
}

ok <- function(Grenzwert) {  
  as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(timeline$Inzidenz >= Grenzwert),])$Meldedatum)))
}
limitslist <- as.data.frame(sapply(Schwellen$Grenzwert, ok))

limitslist

Schwellen$ok_seit <- sapply(Schwellen$Grenzwert, ok)





library(flexdashboard)

gauge(99.3, min = 0, max = 150,  gaugeSectors(
  success = c(0, 50), warning = c(50, 100), danger = c(100, 999)
))

round(tail(timeline,1)$Inzidenz,1)




library(rio)
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile"


tmp <- rio::import(file = url,which = 7)[-(1:4),] %>%
  rename(Kreis=2) %>%
  filter(Kreis=="SK Passau") %>%
  t() %>%
  as.data.frame 
tmp

tmp$...2



library(readxl)
Fallzahlen_Kum_Tab <- read_excel(url, sheet = "LK_7-Tage-Inzidenz", skip = 2) %>% t()
View(Fallzahlen_Kum_Tab)