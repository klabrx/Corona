status <- function(Inzidenz) {
  as.character(
    cut(Inzidenz,
        breaks=c(-Inf,35, 50, 100, 150, 200, Inf),
        labels=c("000-035","035-050","050-100","100-150","150-200","200+")
        )
    )
}