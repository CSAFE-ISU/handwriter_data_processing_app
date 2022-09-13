server <- function(input, output, session) {
  
  #Tooltips
  source(file.path("server", "tooltips.R"), local = TRUE)$value
  
  #========================================================
  #================ SOURCE SERVER CODE ====================
  #========================================================
  
  source(file.path("server", "tooltips.R"), local = TRUE)$value
  source(file.path("server", "process.R"), local = TRUE)$value
  source(file.path("server", "dataIntegrityChecks.R"), local = TRUE)$value
  
}
