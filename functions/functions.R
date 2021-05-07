RKI.cases <- function(AGS, MD_von, MD_bis)
{
  service.url <- paste0("https://services7.arcgis.com/",
                        "mOBPykOjAyBO2ZKk/",
                        "arcgis/rest/services/RKI_COVID19/FeatureServer/0/",
                        "query?where=")
  parameters <- paste(sep="&",
                      paste(sep=" AND ",
                            paste("IdLandkreis = ",AGS),
                            paste("Meldedatum >= TIMESTAMP '",MD_von," 00:00:00'"),
                            paste("Meldedatum <= TIMESTAMP '",MD_bis," 00:00:00'"),
                            "NeuerFall in (-1,0,1)"
                      ),
                      # "outFields=AnzahlFall,Meldedatum",
                      "outFields=AnzahlFall,Meldedatum,Altersgruppe,Geschlecht,NeuerFall,AnzahlTodesfall,NeuerTodesfall",
                      "orderByFieldsForStatistics=Meldedatum",
                      "groupByFieldsForStatistics=Meldedatum",
                      "f=pjson"
  )
  
  # Zusammenfügen von Link und Parametern, URL-taugliche Codierung
  GET.request <- URLencode(paste0(service.url, parameters))
  
  # Hier erfolgt die eigentliche Abfrage
  RKI_data <- fromJSON(GET.request)
  
  # Umwandlung JSON-Format in Dataframe
  cases <- as.data.frame(RKI_data$features$attributes)
  # Umwandlung JSON-Datumsformat in humanlesbar
  cases$Meldedatum <- as.Date(cases$Meldedatum/86400000,origin="1970-01-01")
  
  
  # Rekursion (Minimiert die Anzahl der API-Abrufe *UND* ist datumsunabhängig):
  # WENN der Abruf weniger als 5000 Zeilen lang ist (Limit der API),
  # DANN ist die Funktion durch, 'cases' wird zurückgegeben.
  # SONST: Der letzte evtl. unvollständige Meldetag ('MD') wird entfernt,
  #        ein erneuter Abruf, diesmal beginnend mit MD erfolgt,
  #        die resultierenden Zeilen werden angefügt, bis endlich ein Abruf
  #        mit weniger als 5000 Zeilen erfolgt (Ausstieg aus der Rekursion)
  MD <- max(cases$Meldedatum)
  if (as.numeric(count(cases)) < 5000) return(cases) else
    return(cases %>% filter(Meldedatum < MD) %>%
             bind_rows(RKI.cases(AG,MD,MD_bis))
    )
  return(cases)
}
