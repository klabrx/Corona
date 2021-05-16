

Tagesstatus <- function(data){
  status.est <- status(as.numeric(data[nrow(data),6]))
  if(status.est == Tagesstatus(lag(data))) return(status.est) else return("Unterschied)")
    
}