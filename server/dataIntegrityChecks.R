#=========================================================
#=============== DATA INTEGRITY CHECKS ===================
#=========================================================
volumes = getVolumes()

shinyDirChoose(
  input,
  'dir',
  roots = volumes(),
  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
)

dir <- reactive(input$dir)

output$dir <- renderText({
  global$datapath
})

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$dir
             },
             handlerExpr = {
               global$datapath <- input$dir
             })