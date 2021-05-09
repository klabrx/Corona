
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


dash.rki.data <- rio::import(file = url,which = 7)[-(1:4),] %>%
  rename(Kreis=2, AGS=3) %>%
  filter(Kreis %in% c("SK Passau", "LK Passau")) %>%
  t() %>%
  as.data.frame 
# dash.rki.stand <- row.names(dash.rki.data)[1]
names(dash.rki.data) <- dash.rki.data[2,]


dash.rki.stand <- as.Date(parse_date_time(substring(row.names(dash.rki.data)[1], 8, 17), order="dmy"))


dash.rki.data <- as.data.frame(dash.rki.data[-(1:3),]) %>% # rename(Dashboard.Inzidenz=1)
# dash.rki.data$Dashboard.Inzidenz <- round(as.numeric(dash.rki.data$Dashboard.Inzidenz),1)
dash.rki.data$Datum <- dash.rki.stand - max(row(dash.rki.data)) + row(dash.rki.data)
dash.rki.data %>% tail()

# dashdat erzeugen
# url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile"
# dashdat <- rio::import(file = url,which = 7)[-(1:4),] %>%
#   as.data.frame() %>%
#   rename(Kreis=2) %>%
#   filter(Kreis %in% c("SK Passau")) %>%
#   t() %>%
#   as.data.frame()
# dashdat.stand <- as.Date(parse_date_time(substring(row.names(dashdat)[1], 8, 17), order="dmy"))
# 
# 
# dashdat <- dashdat[-(1:3),] 
# dashdat$Datum <- dashdat.stand - max(row(dashdat)) + row(dashdat)
# dashdat$Meldedatum <- dashdat$Datum-1
# dashdat  <- as.data.frame(as.numeric(dashdat))
# names(dashdat) <- c("Inzidenz.Dashboard")
# dashdat$Meldedatum <- dashdat$Datum-1
# 
# 
# 
# 
# head(dashdat)
# tail(dashdat)
# dashdat.stand
library(readxl)
library(anytime)
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile"
destfile <- "Fallzahlen_Kum_Tab.xlsx"
curl::curl_download(url, destfile)
dashdat <- read_excel(destfile, sheet=7, range="A2", col_names = FALSE) 
dashdat.stand <- as.Date(parse_date_time(substring(dashdat[1,1], 8, 17), order="dmy"))
# names(dashdat[4]) <- c("Datum")

dashdat.stand

dashdat <- read_excel(destfile, sheet=7, skip=4, col_names = FALSE) %>% select(-1) %>% t()  %>% data.frame()
as.excel.date=function(x) as.Date(x,origin='1900-01-01')-2

dashdat[which(nchar(dashdat$X1) == 10),]$X1 <- as.Date(parse_date_time(dashdat[which(nchar(dashdat$X1) == 10),]$X1, c('d.m.y')))
dashdat[which(nchar(dashdat$X1) == 11),]$X1 <- dashdat[which(nchar(dashdat$X1) == 11),]$X1 %>% as.numeric() %>% as.excel.date()
colnames(dashdat) <- paste0(str_pad(dashdat[2,],5,side=c("left"), pad="0"),"",dashdat[1,])

dashdat <- dashdat[-(1:2),]

dashdat$Datum <- as.Date(as.numeric(dashdat$`0LKNRLK`))
dashdat.pivot <- dashdat %>%
  select(-1) %>%
  pivot_longer(!Datum, 
               names_to = "Kreis",
               values_to = "Inzidenz") %>% 
  mutate(Inzidenz = round(as.numeric(Inzidenz),1)) %>%
  separate(Kreis,c("AGS","Kreis"), sep=5)

dashdat.pivot$mysize <- rep(0.5, nrow(dashdat.pivot))
dashdat.pivot$mysize[dashdat.pivot$AGS=="09262"] <- 1

dashdat.plot <- dashdat.pivot %>% filter(Kreis %in% c("SK Passau",
                                                      "LK Passau",
                                                      "SK Landshut",
                                                      "LK Freyung-Grafenau",
                                                      "LK Rottal-Inn",
                                                      "LK Deggendorf")) %>%
  ggplot(aes(x = Datum, y = Inzidenz, group=Kreis, color=Kreis, size=mysize)) + 
  geom_line() +
scale_size(range = c(0.5, 1), guide="none")
dashdat.plot





