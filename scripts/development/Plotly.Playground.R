library(plotly)
timeline.plot <- dashdat.pivot %>%
  filter(grepl("0926.", AGS)) %>%
  filter(Datum>= "2021-01-01") %>%
  plot_ly(x=~Datum, y=~Inzidenz, type='scatter', color=~Kreis, mode='lines')
timeline.plot


library(plotly)

fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = tail(timeline,1)$Inzidenz,
  title = list(text = "Aktuelle 7-Tage-Inzidenz"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = timeline$Inzidenz[nrow(timeline)-1]),
  gauge = list(
    axis =list(range = list(NULL, 200)),
    steps = list(
      list(range = c(0, 35), color = "chartreuse"),
      list(range = c(35, 50), color = "greenyellow"),
      list(range = c(50, 100), color = "yellow"),
      list(range = c(100, 150), color = "orange"),
      list(range = c(150, 200), color = "red")),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = 200))) 
fig <- fig %>%
  layout(margin = list(l=20,r=30))

fig




