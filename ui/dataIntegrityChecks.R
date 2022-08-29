#DATA INGERITY CHECKS TAB

tabPanel("Data Integrity Checks", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        shinyDirButton("dir", "Input directory", "Upload", FALSE),
                       ),
           mainPanel(width = 8,
                     verbatimTextOutput("dir", placeholder = TRUE)  
           )
         ),
)