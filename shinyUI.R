ui <- shinyUI({
  fluidPage(
    shinyjs::useShinyjs(),
    shinyBS:::shinyBSDep,
    add_busy_spinner(spin = "fading-circle"),
    tags$head(tags$script(src = "message-handler.js"), 
              tags$style(HTML("input[type=\"number\"] {width: 80px;}")),
              tags$style(HTML("hr {border-top: 1px solid #000000;}")),
              tags$style(HTML('#save_document{background-color:#33ADFF} #save_document:hover{background-color:#3398FF} #save_document{color:white}')),
              tags$style(HTML('#save_document_extract{background-color:#33ADFF} #save_document_extract:hover{background-color:#3398FF} #save_document_extract{color:white}')),
              tags$style(HTML('#save_batch{background-color:#33ADFF} #save_batch:hover{background-color:#3398FF} #save_batch{color:white}')),
              tags$style(HTML('#save_mask{background-color:#33ADFF} #save_mask:hover{background-color:#3398FF} #save_mask{color:white}'))),
    
    navbarPage(
      title = "handwriter Data Processing Shiny App",
      
      source(file.path("ui", "intro.R"), local = TRUE)$value,
      source(file.path("ui", "process.R"), local = TRUE)$value,
      source(file.path("ui", "dataIntegrityChecks.R"), local = TRUE)$value
    )
  )
})
