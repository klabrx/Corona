data <- dashdat.pivot %>%
  filter(Kreis == "SK Passau")
inz.akt <- data[,4] %>% tail(1)



attach(data)
above.x.since=function(x) tail(data[which(Inzidenz < x ),1],1) + 1
above.x.since(35)[1,1]
above.x.since(50)[1,1]
above.x.since(100)[1,1]
above.x.since(150)[1,1]
below.x.since=function(x) tail(data[which(Inzidenz > x ),1],1) + 1
below.x.since(35)[1,1]
below.x.since(50)[1,1]
below.x.since(100)[1,1]
below.x.since(150)[1,1]
detach(data)




# Parsen des Ergebnisstrings
reply.over <- paste0("Inzidenz: ",
                inz.akt,
                " - Der Grenzwert 50 somit überschritten (seit ",
                format(as.Date(above.x.since(50)[1,1]), "%A, %d.%m.%Y"),
                "). Vorgesehener Bekanntmachungstermin: ",
                format(as.Date(above.x.since(50)[1,1] + 2), "%A, %d.%m.%Y"),
                ", wirksam ab ", format(as.Date(above.x.since(50)[1,1] + 4), "%A, %d.%m.%Y"),".")
reply.over

reply.under <- paste0("Inzidenz: ",
                     inz.akt,
                     " - Der Grenzwert 100 somit eingehalten (seit ",
                     format(as.Date(below.x.since(100)[1,1]), "%A, %d.%m.%Y"),
                     "). Vorgesehener Bekanntmachungstermin: ",
                     format(as.Date(below.x.since(100)[1,1] + 4), "%A, %d.%m.%Y"),
                     ", wirksam ab ", format(as.Date(below.x.since(100)[1,1] + 6), "%A, %d.%m.%Y"),".")
reply.under

reply.old <- paste0("Inzidenz: ",
                      inz.akt,
                      " - Der Grenzwert 100 ist seit ",
                      format(as.Date(below.x.since(100)[1,1]), "%A, %d.%m.%Y"),
                      " eingehalten. Die vorgeschriebene Bekanntmachung erfolgte am ",
                      format(as.Date(below.x.since(100)[1,1] + 4), "%A, %d.%m.%Y"),
                      ", und wurde am ", format(as.Date(below.x.since(100)[1,1] + 6), "%A, %d.%m.%Y")," wirksam.")
reply.old



above.x.since(50)
below.x.since(50)

# above.35.since <- tail(data[which(Inzidenz < 35 ),1],1) + 1
# above.35.since




dash.cases <- read_excel(destfile, sheet=6, skip=4, col_names = TRUE)  %>% select(-1) 



tmp %>% 
  # grouping key(s):
  group_by(AGS) %>%
  # check if there is any value change
  # if yes, a new sequence id is generated through cumsum
  mutate(last_one = lag(Stat.est, 1), 
         not_eq = last_one != Stat.est, 
         seq = cumsum(not_eq)) %>% 
  # the following is just to find the largest sequence
  count(AGS, Stat.est, seq) %>% 
  group_by(Datum, Stat.est) %>% 
  summarise(max_consecutive_event = max(n))