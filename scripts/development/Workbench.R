Altersstruktur <- cases %>%
  filter(NeuerFall %in% c(0,1)) %>%
  filter(Meldedatum > Sys.Date() - 8) %>%
  group_by(Altersgruppe) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  pivot_wider(names_from = Altersgruppe, values_from = AnzahlFall) %>%
  as.data.frame()

Altersstruktur
sum(Altersstruktur$AnzahlFall)



weekdays %>%
  mutate(Mittelwert = Fallzahl_Durchschnitt) %>%
  arrange(DoW) %>%
  select(1,4) %>%
  print(row.names = FALSE)

CasePlot <- cases %>%
  filter(NeuerFall %in% c(0,1)) %>%
  filter(Meldedatum > "2021-01-01") %>%
  group_by(Meldedatum) %>%
  summarise(Fälle = sum(AnzahlFall)) %>%
  # pivot_wider(names_from = Meldedatum, values_from = AnzahlFall) %>%
  as.data.frame() %>%
  ggplot(aes(x = Meldedatum, y = Fälle)) +
  geom_col() + 
  geom_smooth(method = "gam") + 
  labs(title=paste0("Tägliche Neufälle seit 01.01.2021 mit Trendlinie"))
CasePlot



library(readr)
Dashboard_Inzidenzen <- read_delim("data/Dashboard-Inzidenzen.csv",
                                   ";", escape_double = FALSE, col_types = cols(Tag = col_date(format = "%d.%m.%Y")),
                                   locale = locale(decimal_mark = ",", grouping_mark = "."),
                                   trim_ws = TRUE)
names(Dashboard_Inzidenzen) <- c("Meldedatum", "Dashboardwert")
merge(timeline,Dashboard_Inzidenzen)
Vergleich <- merge(timeline,Dashboard_Inzidenzen)
Vergleich$Diff <- Vergleich$Dashboardwert - Vergleich$Inzidenz
Vergleich$Abweichung <- round(Vergleich$Diff/Vergleich$Inzidenz*100,1)
head(Vergleich)

Vergleichsplot <- ggplot(Vergleich, aes(x = Meldedatum, y = Inzidenz)) +
  geom_line(col = "green", size = 3) +
  geom_line(aes(y = Dashboardwert), col = "red")
Vergleichsplot

with(Vergleich, cor(Inzidenz, Dashboardwert))

reg <- lm(Vergleich$Inzidenz ~ Vergleich$Dashboardwert)
tail(Vergleich)
mean(Vergleich$Diff)
mean(Vergleich$Abweichung)

ggplot(Vergleich, aes(x = Inzidenz, y = Dashboardwert)) + 
  geom_point() +
  geom_abline()


attach(timeline)
ok <- function(Grenzwert){
  result <- as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(Inzidenz >= Grenzwert),])$Meldedatum)))
  if(result == 0) return("Nicht unterschritten") else
    if(result == 1) return("seit 1 Tag unterschritten") else
      return(paste0("seit ",result," Tagen unterschritten"))
}
detach(timeline)
ok(80)
data.dashboard <- as.data.frame(0)
edit(data.dashboard)


# Todesfälle-----------------------------------------------
deaths <- cases %>% 
  filter (NeuerTodesfall %in% c(0,1)) %>% 
  group_by(Altersgruppe, Meldedatum) %>% 
  summarise(Todesfälle = sum(AnzahlTodesfall))
deaths


cases %>%
  filter(NeuerTodesfall %in% c(1,0)) %>%
  group_by(Meldedatum, Altersgruppe)  %>% 
  summarise_at(c("AnzahlTodesfall"), sum, na.rm = TRUE) %>%
  ggplot(aes(x=Meldedatum, y=AnzahlTodesfall)) +
  geom_area(stat="identity") +
  facet_grid(rows = vars(Altersgruppe))

# Chord-Diagramm ---------------------------------------------------------------
UMZ2020 <- read_delim("~/Downloads/UMZ2020.csv",
";", escape_double = FALSE, col_types = cols(Stadtteil = col_skip()),
trim_ws = TRUE)
umz <- as.matrix(UMZ2020)



library(chorddiag)
library(viridis)

m <- umz
Stadtteile <- c("Altstadt",
                "Auerbach",
                "Grubweg",
                "Hacklberg",
                "Haidenhof Nord",
                "Haidenhof Süd",
                "Hals",
                "Heining",
                "Innstadt",
                "Kohlbruck",
                "Neustift",
                "Patriching",
                "Rittsteig",
                "Schalding l.d.D.",
                "Schalding r.d.D.",
                "St. Nikola")
dimnames(m) <- list(have = Stadtteile,
                    prefer = Stadtteile)

groupColors <- viridis_pal(option = "C")(16)
chorddiag(m, groupColors = groupColors, groupnamePadding = 25)


# sankey -----------------------------------------------------------------------
# Load package
library(networkD3)

# Load energy projection data
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)


# Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
head( Energy$links )
head( Energy$nodes )

# Thus we can plot it
p <- sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 12, nodeWidth = 30)
p

# save the widget
# library(htmlwidgets)


# collapsible tree -------------------------------------------------------------
# Load library
# install.packages("collapsibleTree")
library(collapsibleTree) 

# input data must be a nested data frame:
head(warpbreaks)

# Represent this tree:
p <- collapsibleTree( warpbreaks, c("wool", "tension", "breaks"))
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dendrogram_interactive.html"))