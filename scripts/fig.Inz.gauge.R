Inz.gauge.fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value  = tail(timeline,1)$Inzidenz,
  title  = list(text = "Aktuelle 7-Tage-Inzidenz"),
  type   = "indicator",
  mode   = "gauge+number+delta",
  delta  = list(reference = timeline$Inzidenz[nrow(timeline)-1],
                increasing = list(color = "red"),
                decreasing = list(color = "green")),
  gauge  = list(
    axis = list(range = list(NULL, 200),
                tickmode = "array",
                tickvals = list(0,35,50,100,150,200),
                ticktext = list("0","35","50","100","150","200")),
    bar = list(color = "#555"),
    steps = list(
      list(range = c(0, 35), color = "green"),
      list(range = c(35, 50), color = "greenyellow"),
      list(range = c(50, 100), color = "yellow"),
      list(range = c(100, 150), color = "orange"),
      list(range = c(150, 200), color = "red")),
    threshold = list(
      line = list(color = "red",
                  width = 4),
      thickness = 0.75,
      value = 200
      )
    )
  ) 
Inz.gauge.fig <- Inz.gauge.fig %>%
  layout(margin = list(l=20,r=30))

# Inz.gauge.fig