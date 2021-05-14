# Hier erfolgen zwei getrennte Datenabrufe:
# 1. Der Abruf des aktuellen Infektionsgeschehens incl. aller Nachmeldungen
#    aus der RKI-Datenbank (tagesaktuell ab ca. 02:00), sowie
# 2. der Download einer Excel-Datei von RKI, die die im Dashboard jeweils ver-
#    öffentlichten Zahlen ausweist. Darin sind die Nachmeldungen **NICHT** ent-
#    halten; jedoch sind diese Zahlen für die "inzidenzabhängigen Regelungen"
#    im Rahmen der verschiedenen "Notbremsenmaßnahmen" maßgeblich.

# RKI-Datenbankzahlen (incl. Nachmeldungen)
cases <- RKI.cases(params$IDKreis, "2020-01-01", Sys.Date()) 

# "Eingefrorene" RKI-Dashboardzahlen (ohne Nachmeldungen, notbremsenrelevant)
url <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile"
destfile <- "Fallzahlen_Kum_Tab.xlsx"
curl::curl_download(url, destfile)