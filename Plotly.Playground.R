library(plotly)
timeline.plot <- dashdat.pivot %>%
  filter(grepl("0926.", AGS)) %>%
  filter(Datum>= "2021-04-01") %>%
  plot_ly(x=~Datum, y=~Inzidenz, type='scatter', color=~Kreis, mode='lines')
timeline.plot