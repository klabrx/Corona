# Funktion zählt die Differenz in Tagen vom jüngsten Meldedatum überhaupt bis
# zum letzten Datum, an dem ein anzugebender Grenzwert überschritten war
# Parameter: Grenz-/Schwellenwert
ok <- function(Grenzwert) {  
  as.numeric((max(timeline$Meldedatum) - max(as.data.frame(timeline[which(timeline$Inzidenz >= Grenzwert),])$Meldedatum)))
}