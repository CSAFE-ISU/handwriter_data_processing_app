#DATA INGERITY CHECKS TAB

tabPanel("Data Integrity Checks", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        h2("Data Integrity Checks"),
                        br(),
                        h4("Select Folder:")
                       ),
           mainPanel(width = 8,
                     span(textOutput("error"), style="color:red")
           )
         ),
)