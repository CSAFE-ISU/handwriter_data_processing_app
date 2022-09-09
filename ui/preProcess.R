#PREPROCESS TAB

tabPanel("Pre-process", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        h2("Pre-process"),
                        fileInput("upload", "Choose a new document to process", accept = c('image/png')),
                        hr(),
                        h4("Current document:"),
                        fluidRow(column(width=11, offset=1, textOutput("qr"))),
                        fluidRow(column(width=11, offset=1, textOutput("doc_type"))),
                        fluidRow(column(width=11, offset=1, textOutput("writer"))),
                        
                        # Survey or writing only panel
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: survey' || output.doc_type == 'Document Type: writing'",
                          fluidRow(column(width=11, offset=1, textOutput("session"))),
                        ),
                        
                        # Prompt only panel
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: writing'",
                          fluidRow(column(width=11, offset=1, textOutput("prompt"))),
                          fluidRow(column(width=11, offset=1, textOutput("repetition"))),
                          ),
                        
                        # Signature only panel
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: signature'",
                          fluidRow(column(width=11, offset=1, textOutput("initials"))),
                        ),
                        
                        # QR Code
                        fluidRow(column(width=11, offset=1, actionButton("select_qr", "Select QR code manually"))),
                        fluidRow(column(width = 11, offset=1, actionButton("save_scan", "Save Original Scan"))),
                        hr(),
                        
                        # Writing and signature only panel - crop and rotate
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
                        
                        # Survey only panel - survey responses
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: survey'",
                          h4("Survey Responses:"),
                          textOutput("csv_path"),
                          fluidRow(column(width=11, offset=1, textInput("response_initials", "What are your initials?", ""))),
                          fluidRow(column(width=11, offset=1, textInput("response_location", "What is your current location?", ""))),
                          fluidRow(column(width=11, offset=1, radioButtons("response_time", 
                                                                           "Which best describes the current time?", 
                                                                           choices = c("a. Early morning (earlier than 9:30am)",
                                                                                            "b. Late morning (9:30am-12:00pm)",
                                                                                            "c. Early afternoon (12:00pm-2:30pm)",
                                                                                            "d. Late afternoon (2:30pm-5pm)",
                                                                                            "e. Early evening (5pm-7:30pm)",
                                                                                            "f. Late evening (later than 7:30pm)")))),
                          fluidRow(column(width=11, offset=1, dateInput("response_date", "What is the date today?")))
                          ),  # end conditionalPanel
                        
                        # Survey 1 only panel - survey responses
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: survey' && output.session == 'Session: 1'",
                          fluidRow(column(width=11, offset=1, textInput("response_3rd_grade", "Location of your 3rd grade education?", ""))),
                          fluidRow(column(width=11, offset=1, radioButtons("response_age", 
                                                                           "In which age range do you fall?", 
                                                                           choices = c("a. 18-24", "b. 25-40", "c. 41-60", "d. 61+")))),
                          fluidRow(column(width=11, offset=1, radioButtons("response_language", 
                                                                           "Do you consider English to be your first language?", 
                                                                           choices = c("yes", "no")))),
                          fluidRow(column(width=11, offset=1, radioButtons("response_gender", 
                                                                           "What is your gender identity?", 
                                                                           choices = c("a. Female", "b. Male", "c. Other")),
                                          # only show if other is selected for gender identity
                                          conditionalPanel(condition = "input.response_gender == 'c. Other'",
                                                           helpText("Type the participant's response for 'other'"),
                                                           textInput("response_gender_other", "", "")))),
                          fluidRow(column(width=11, offset=1, radioButtons("response_ethnicity", 
                                                                           "Which best describes your ethnicity?",
                                                                           choices = c("a. African American", "b. Asian", "c. Caucasian", "d. Hispanic", "e. Native American", "f. South Pacific", "g. Other")))),
                          fluidRow(column(width=11, offset=1, radioButtons("response_education_level", 
                                                                           "Which of the following best describes your highest level of formal education?", 
                                                                           choices = c("a. High school or less", "b. More than high school")))),
                          fluidRow(column(width=11, offset=1, radioButtons("response_hand", 
                                                                           "Which hand do you use to write?", 
                                                                           choices = c("a. Left", "b. Right", "c. Ambidextrous")))),
                          
                          ),  # end conditionalPanel
                        
                        # Survey only panel - survey responses
                        conditionalPanel(
                          condition = "output.doc_type == 'Document Type: survey'",
                          fluidRow(column(width=11, offset=1, actionButton("save_survey", "Save Survey Reponses"))),
                        ),  # end conditionalPanel
                        ),
           mainPanel(width = 8,
                     span(textOutput("error"), style="color:red"),
                     br(),
                     tabsetPanel(id = "plotset",
                                 tabPanel("Current Document",
                                          # Survey only panel
                                          conditionalPanel(
                                            condition = "output.doc_type == 'Document Type: survey'",
                                            tableOutput("survey_table")
                                          ),
                                          # Survey1 only panel
                                          conditionalPanel(
                                            condition = "output.doc_type == 'Document Type: survey' && output.session == 'Session: 1'",
                                            tableOutput("survey1_table")
                                          ),
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
                                 ),
                                 tabPanel("Data Checks",
                                          h4("Missing Documents"),
                                          dataTableOutput("docs_missing"),
                                          h4("Processed Documents"),
                                          dataTableOutput("docs_processed"),
                                 )
                     ),
           )
         ),
)