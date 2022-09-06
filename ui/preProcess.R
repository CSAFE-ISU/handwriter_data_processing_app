#PREPROCESS TAB

tabPanel("Pre-process", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        h2("Pre-process"),
                        fileInput("upload", "Choose a new document to process", accept = c('image/png')),
                        hr(),
                        h4("Current document:"),
                        fluidRow(column(width=11, offset=1, textOutput("dimensions"))),
                        fluidRow(column(width=11, offset=1, textOutput("qr"))),
                        fluidRow(column(width=11, offset=1, textOutput("doc_type"))),
                        fluidRow(column(width=11, offset=1, textOutput("writer"))),
                        # Only show this panel if the doc type is a survey
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: survey' || output.doc_type == 'Document Type: writing'",
                          fluidRow(column(width=11, offset=1, textOutput("session"))),
                        ),
                        # Only show this panel if the doc type is a prompt
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: writing'",
                          fluidRow(column(width=11, offset=1, textOutput("prompt"))),
                          fluidRow(column(width=11, offset=1, textOutput("repetition"))),
                          ),
                        # Only show this panel if the doc type is a signature
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: signature'",
                          fluidRow(column(width=11, offset=1, textOutput("initials"))),
                        ),
                        fluidRow(column(width=11, offset=1, actionButton("select_qr", "Select QR code manually"))),
                        fluidRow(
                          column(width = 11, offset=1, actionButton("save_scan", "Save Original Scan")),
                        ),
                        hr(),
                        
                        # only show crop and rotate options for writing and signatures
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: signature' || output.doc_type == 'Document Type: writing'",
                          h4("Rotate:"),
                          fluidRow(
                            column(width = 1, br(), actionButton("left", label = icon("angle-double-left", "fa-2xs"))),
                            column(width = 8, offset = 1, sliderInput("rotation", "Rotate:", min = -180, max = 180, value = 0)), 
                            column(width = 1, br(), actionButton("right", label = icon("angle-double-right", "fa-2xs"))),
                          ), 
                          hr(),
                          h4("Crop:"),
                          fluidRow(
                            column(width = 4, actionButton("reset_crop", "Reset Crop")),
                            column(width = 4, actionButton("undo_crop", "Undo Crop")),
                            column(width = 4, actionButton("crop", "Crop Area")),
                          ),
                          hr(),
                          br(),
                          fluidRow(
                            column(width = 12, actionButton("save_crop", "Save Current Document")),
                          )),
                        # only show survey response fields for surveys
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: survey'",
                          h4("Survey Responses:"),
                          fluidRow(column(width=11, offset=1, textInput("response_initials", "Initials", ""))),
                          fluidRow(column(width=11, offset=1, textInput("response_location", "Current location", ""))),
                          fluidRow(column(width=11, offset=1, radioButtons("time", 
                                                                           "Current time", 
                                                                           choiceValues = c("a", "b", "c", "d", "e", "f"),
                                                                           choiceNames = c("Early morning (earlier than 9:30am)",
                                                                                            "Late morning (9:30am-12:00pm)",
                                                                                            "Early afternoon (12:00pm-2:30pm)",
                                                                                            "Late afternoon (2:30pm-5pm)",
                                                                                            "Early evening (5pm-7:30pm)",
                                                                                            "Late evening (later than 7:30pm)")))),
                        )
                        ),
           mainPanel(width = 8,
                     span(textOutput("error"), style="color:red"),
                     br(),
                     tabsetPanel(id = "plotset",
                                 tabPanel("Current Document",
                                          br(),
                                          imageOutput("preprocess_plot", 
                                                      brush = brushOpts(id = "preprocess_plot_brush", resetOnNew = TRUE))
                                 ),
                                 tabPanel("Apply Mask",
                                          br(),
                                          fluidRow(
                                            column(width = 2, actionButton("mask", "Mask Area")),
                                            column(width = 2, actionButton("undo_mask", "Undo Last Mask")),
                                            column(width = 2, actionButton("reset_mask", "Remove Mask")),
                                            column(width = 2, downloadButton("save_mask", "Save Mask"))
                                            
                                          ),
                                          hr(),
                                          imageOutput("preprocess_plot_masked", brush = brushOpts(id = "preprocess_plot_brush", resetOnNew = TRUE))
                                 )
                     ),
           )
         ),
)