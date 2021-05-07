rmarkdown::render(
  input = "Report.rmd",
  envir = .GlobalEnv,
  # output_format = "word_document",
  output_file = paste0("./html/RKI_Report_09262-", Sys.Date() - 1, ".html"),
  output_dir = paste0("html/", "09262")
)

