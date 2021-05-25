gauge.lim <- function(Grenzwert) {

fig <- gauge(as.numeric(tail(dash.AGS,1)$Datum - max(dash.AGS[which(dash.AGS$Inzidenz >= Grenzwert),]$Datum)),
             min = 0,
             max = 10,
             symbol = ' T',
             gaugeSectors(success = c(3, 999),
                          warning = c(1, 3),
                          danger = c(0, 0)
                          )
             )
return(fig)
}