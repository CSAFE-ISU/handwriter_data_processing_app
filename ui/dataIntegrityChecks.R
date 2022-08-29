#DATA INGERITY CHECKS TAB

tabPanel("Data Integrity Checks", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        h2("Data Integrity Checks"),
                        br(),
                        h4("Current document:"),
                        fluidRow(column(width=11, offset=1, textOutput("image_name"))),
                        fluidRow(column(width=11, offset=1, textOutput("dimensions"))),
                       ),
           mainPanel(width = 8,
                     span(textOutput("error"), style="color:red"),
                     br(),
                     tabsetPanel(id = "plotset",
                                 tabPanel("Current Document",
                                          br(),
                                 ),
                                 tabPanel("Apply Mask",
                                          br(),
                                 )
                     ),
                     
           )
         ),
)