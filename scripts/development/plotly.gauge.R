library(plotly)
fig <- plot_ly(domain = list(x = c(0, 1),
                             y = c(0, 1)
                             ),
               value  = 55.3,
               title  = list(text = "Title"),
               type   = "indicator",
               mode   = "gauge+number+delta",
               delta  = list(reference  = 53,
                             increasing = list(color = "red"),
                             decreasing = list(color = "green")
                             ),
               gauge  = list(axis = list(range    = list(-200, 200),
                                         tickmode = "array",
                                         tickvals = list(-200,-100,-50,-35,
                                                         0, 35, 50, 100, 200),
                                         ticktext = list("-200","-100","-50",
                                                         "-35","0","35","50",
                                                         "100","150","200")
                                         ),
                             bar = list(color = "#555"),
                             steps = list(list(range = c(-200, -50), color = "red"),
                                          list(range = c(-50, -35),  color = "orange"),
                                          list(range = c(-35, 0),    color = "yellow"),
                                          list(range = c(0, 100),    color = "greenyellow"),
                                          list(range = c(0, 200),    color = "green")
                                          ),
                             threshold = list(line = list(color = "red",
                                                          width = 4),
                                              thickness = 0.75,
                                              value = 50) # end threshold
                             ) #end gauge
               ) # end plotly
fig 