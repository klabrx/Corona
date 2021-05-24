rmarkdown::render(
  input = "Report.rmd",
  envir = .GlobalEnv,
  output_file = paste0("./html/RKI_Report_09262-", Sys.Date() - 1, ".html"),
  output_dir = paste0("html/", "09262")
)
rmarkdown::render(
  input = "FlexDash.rmd",
  envir = .GlobalEnv,
  output_file = paste0("./html/Flexdashboard-", Sys.Date(), ".html"),
  output_dir = paste0("html/", "09262")
)


print(paste0("Sys.time: ",Sys.time()," - Datenstand:", parse_date_time(cases[1,8], c('d.m.y, H:M')))) 
