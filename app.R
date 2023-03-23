#Shiny handwriter
# devtools::install_github("CSAFE-ISU/handwriter")
# devtools::install_github("CSAFE-ISU/quadrangle", INSTALL_opts = "--no-multiarch")

library(magick)
library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyBS)
library(shinyFiles)
library(DT)
library(stringr)
library(dplyr)
library(quadrangle)

print(paste0('working in: ', getwd()))

source('shinyUI.R', local = TRUE)
source('shinyServer.R')

shinyApp(ui, server)
