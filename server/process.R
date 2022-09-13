#=================== PREPROCESSING =======================


# Initial Setup -----------------------------------------------------------

# disable buttons
shinyjs::disable("reset_crop") 
shinyjs::disable("undo_crop") 
shinyjs::disable("save_mask")
shinyjs::disable("reset_mask") 
shinyjs::disable("undo_mask")
shinyjs::disable("save_scan")

# create reactive values
values <- reactiveValues()

# read in sample image
image <- magick::image_read("images/samplewriting.png")
values$image <- image 
values$uploaded_image <- image
values$image_name <- 'images/samplewriting.png'
values$upload_path <- "images/samplewriting.png"
values$current_path <- "images/samplewriting.png"
values$doc_type <- "default"

# image info
info <- image_info(image)
values$info <- info
values$dimensions <- paste0(info$width, 'x', info$height)

# masking
mask_list_df <- data.frame(matrix(ncol=6, nrow=0))
colnames(mask_list_df) <- c('xmin', 'xmax', 'ymin', 'ymax', 'xrange', 'yrange')
values$mask_list_df <- mask_list_df

# cropping
values$crop_list <- list(image)

# qr code
qr <- reactiveValues()
qr$code <- NULL

# data checks 
data <- reactiveValues()
data$df <- data.frame(matrix(nrow=0, ncol=3,dimnames=list(NULL, c("full_path", "doc_type", "file"))))

# main directory based on OS (Darwin = Mac)
switch(Sys.info()[['sysname']],
       Darwin = {values$main_dir = "/Volumes/lss/research/csafe-handwriting-irb/Data_Processing_App_Testing"},
       Windows = {
         # Check whether csafe-handwriting-irb is a mounted folder
         if (dir.exists("/lss/research/csafe-handwriting-irb/Data_Processing_App_Testing")){
           values$main_dir = "/lss/research/csafe-handwriting-irb/Data_Processing_App_Testing"
         } else if (dir.exists("Y:/Data_Processing_App_Testing")){
           values$main_dir = "/lss/research/csafe-handwriting-irb/Data_Processing_App_Testing"
         }
       },
)

# create subdirectory temp in images 
if (!dir.exists(file.path("images", "temp"))){
  dir.create(file.path("images", "temp"))
}


# Image -------------------------------------------------------------------
#UPLOAD: document
observeEvent(input$upload, {
  # turn off save scan button
  shinyjs::disable("save_scan")
  
  # update upload_path
  if (length(input$upload$datapath)){
    values$upload_path <- input$upload$datapath
  }
  
  # reset 
  values$plot_type <- ''
  values$uploaded_image <- NULL
  data$df <- data.frame(matrix(nrow=0, ncol=3,dimnames=list(NULL, c("full_path", "doc_type", "file"))))
  
  # reset survey responses input boxes
  updateTextInput(session, "response_initials", value = "")
  updateTextInput(session, "response_location", value = "")
  updateRadioButtons(session, "response_time", selected = "a. Early morning (earlier than 9:30am)")
  updateDateInput(session, "response_date", value = NULL)
  updateTextInput(session, "response_3rd_Grade", value = "")
  updateRadioButtons(session, "response_age", selected = "a. 18-24")
  updateRadioButtons(session, "response_language", selected = "yes")
  updateRadioButtons(session, "response_gender", selected = "a. Female")
  updateTextInput(session, "response_gender_other", value = "")
  updateRadioButtons(session, "response_ethnicity", selected = "a. African American")
  updateRadioButtons(session, "response_education_level", selected = "a. High school or less")
  updateRadioButtons(session, "response_hand", selected = "a. Left")
  
  # read image
  if(endsWith(input$upload$datapath, "png")){
    values$uploaded_image <- image_read(input$upload$datapath)
    values$image <- values$uploaded_image
    values$info <- image_info(values$image)
  }else if(endsWith(input$plot_upload$datapath, "RData") || endsWith(input$plot_upload$datapath, "rda")){
    image_with_mask <- load(input$upload$datapath)
    values$uploaded_image <- image_read(magick_image)
    values$image <- values$uploaded_image
    values$info <- image_info(values$image)
  }
  
  # update current document info
  values$image_name <- input$upload$name
  
  # reset crop list and mask list
  values$crop_list <- list(values$image)
  values$mask_list_df <- values$mask_list_df[0,]  # keep column names, clear all rows
  
  # reset qr code reactive values in case qr code doesn't read automatically on upload
  values$doc_type <- "default"
  qr$writer <- qr$session <- qr$prompt <- qr$repetition <- qr$initials <- qr$scan_name <- qr$scan_path <- qr$crop_name <- qr$crop_path <- qr$qr_path <- NULL
  
  # read qr code and get document info
  qr$code <- quadrangle::qr_scan(values$image)$values$value  # read qr code
  # if qr code isn't empty, format doc names
  if (length(qr$code) != 0){
    # get info from qr code
    splitQR(qr$code)
    # format document names
    makeDocNames()
    # enable save scan button
    shinyjs::enable("save_scan")
    # get list of docs for current writer
    data$df <- listAllDocs()
    # Find missing docs
    data$missing <- data$df[!file.exists(data$df$full_path),]
    # Find processed docs
    data$processed <- data$df[file.exists(data$df$full_path),]
  }
  
  # extract number from writer id for survey response table
  if (values$doc_type == 'survey'){
    id <- stringr::str_extract(qr$writer, "\\d+")
    survey$df['WID'] <- as.integer(id)
    survey$df['Session'] <- qr$session
  }
})

#RENDER: image
output$preprocess_plot <- renderImage({
  output$error <- renderText({""})
  
  values$session_width = session$clientData$output_preprocess_plot_width
  values$session_height = session$clientData$output_preprocess_plot_height
  values$session_scale = values$session_width / values$info$width
  values$session_inv_scale = values$info$width / values$session_width
  
  # make temp image for display
  if (values$doc_type == 'survey'){
    tmp <- values$image %>%
      image_resize(geometry_size_pixels(height=2.25*values$session_height)) %>%
      image_write(tempfile(fileext='png'), format = 'png')
    list(src = tmp, contentType = "image/png", height=2.25*values$session_height)
  } else {
    tmp <- values$image %>%
      image_rotate(input$rotation) %>%
      image_resize(geometry_size_pixels(width=values$session_width)) %>%
      image_write(tempfile(fileext='png'), format = 'png')
    list(src = tmp, contentType = "image/png", width=values$session_width)
  }
}, deleteFile = FALSE)


# Original Scan -----------------------------------------------------------
#HELPER FUNCTION: save original scan
saveScan <- function(){
  # make writer folder for scan
  if (!dir.exists(dirname(qr$scan_path))){
    dir.create(dirname(qr$scan_path))
  }
  
  # save original scan
  values$uploaded_image %>% 
    image_rotate(input$rotation) %>% 
    image_write(path=qr$scan_path, format = 'png')
}


# Survey -----------------------------------------------------------------
#CREATE: survey reactive values
survey <- reactiveValues(
  # all surveys
  df = data.frame(
    "WID" = "",
    "Session" = "",
    "Initials" = "",
    "Location" = "",
    "Time" = "",
    "Date" = ""),
  # survey1 only
  df1 = data.frame(
    "ThirdGradeLoc" = NA,
    "Age" = NA,
    "Language" = NA,
    "Gender" = NA,
    "Other" = NA,
    "Ethnicity" = NA,
    "Edu" = NA,
    "Hand" = NA
  )
)

#HELPER FUNCTION: saveResponses
saveResponses <- function(){
  # make writer folder for csv
  if (!dir.exists(dirname(qr$csv_path))){
    dir.create(dirname(qr$csv_path))
  }
  
  # combine dataframes 
  df = cbind(survey$df, survey$df1)
  
  # set session 1 columns to NA for session 2 and 3 responses
  if (qr$session != 1){
    df[,colnames(survey$df1)] <- NA
  }
  
  # save csv
  write.csv(df, file = qr$csv_path, row.names = FALSE)
  
  # load or create master speadsheet
  qr$master_name <- "survey_responses.csv"
  qr$master_path <- file.path(values$main_dir, "Stage3_Survey_Data", "Spreadsheets", qr$master_name)
  if (file.exists(qr$master_path)){
    # load
    master <- read.csv(qr$master_path)
    # add new responses
    master <- rbind(master, df)
  } else {
    # make master current responses
    master <- df
  }
  
  write.csv(master, file = qr$master_path, row.names = FALSE)
  
}

#HELPER FUNCTION: addMetadata
# Everytime a user clicks saveCrop or saveSurvey, the app checks to see if all
# docs for the current writer have been processed. If they have, the app runs 
# the addMetadata function to copy the current writer's survey responses from 
# survey_responses.csv and add them to metadata.csv.
addMetadata <- function(){
  # return error if current writer is missing documents
  if (nrow(data$missing) > 0){
    output$error <- renderText({"This writer cannot be added to the metadata file because one or more files are missing."})
    return()
  }
  
  # clear error message
  output$error <- renderText({""})
  
  # get writer id 
  id <- stringr::str_extract(qr$writer, "\\d+")
  id <- as.integer(id)
  
  # load survey responses spreadsheet
  qr$master_name <- "survey_responses.csv"
  qr$master_path <- file.path(values$main_dir, "Stage3_Survey_Data", "Spreadsheets", qr$master_name)
  df <- read.csv(qr$master_path)
  
  # filter for writer 
  df <- df[df$WID == id,]
  
  # format file path
  qr$metadata_name <- "metadata.csv"
  qr$metadata_path <- file.path(dirname(qr$master_path), qr$metadata_name)

  # load or create metadata spreadsheet
  if (file.exists(qr$metadata_path)){   
    metadata <- read.csv(qr$metadata_path)
    # add to metadata
    metadata <- rbind(metadata, df)
    
    # check to make sure that writer isn't already in the spreadsheet
    check <- metadata[metadata$WID == id,]
    if (nrow(check) > 0){
      output$error <- renderText({"This writer has already been added to metadata.csv."})
      return()
    }
  } else {
    # create metadata
    metadata <- df
  }
  
  write.csv(metadata, file = qr$metadata_path, row.names = FALSE)
}

#UPDATE: survey values
observeEvent(input$response_initials, {survey$df['Initials'] <- input$response_initials})
observeEvent(input$response_location, {survey$df['Location'] <- input$response_location})
observeEvent(input$response_time, {survey$df['Time'] <- input$response_time})
observeEvent(input$response_date, {survey$df['Date'] <- as.character(input$response_date)})

#UPDATE: survey1 only values
observeEvent(input$response_3rd_grade, {survey$df1['ThirdGradeLoc'] <- input$response_3rd_grade})
observeEvent(input$response_age, {survey$df1['Age'] <- input$response_age})
observeEvent(input$response_language, {survey$df1['Language'] <- input$response_language})
observeEvent(input$response_gender, {survey$df1['Gender'] <- input$response_gender})
observeEvent(input$response_gender_other, {survey$df1['Other'] <- input$response_gender_other})
observeEvent(input$response_ethnicity, {survey$df1['Ethnicity'] <- input$response_ethnicity})
observeEvent(input$response_education_level, {survey$df1['Edu'] <- input$response_education_level})
observeEvent(input$response_hand, {survey$df1['Hand'] <- input$response_hand})

#RENDER: survey table
output$survey_table <- renderTable({
  survey$df
})

#RENDER: survey1 table
output$survey1_table <- renderTable({
  survey$df1
})

#SAVE: survey
observeEvent(input$save_survey, {
  if (file.exists(qr$scan_path) && file.exists(qr$csv_path)){
    output$error <- renderText({paste0("Scan and csv already exist. Manually delete files if you want to save an updated version.")})
  } else if (!file.exists(qr$scan_path) && file.exists(qr$csv_path)) { 
    output$error <- renderText({"Csv already exists. Manually delete file if you want to save an updated version."})
    saveScan()
    # Update missing docs
    data$missing <- data$df[!file.exists(data$df$full_path),]
    # Update processed docs
    data$processed <- data$df[file.exists(data$df$full_path),]
  } else if (file.exists(qr$scan_path) && !file.exists(qr$csv_path)){
    output$error <- renderText({"Scan already exists. Manually delete file if you want to save an updated version."})
    saveResponses()
    # Update missing docs
    data$missing <- data$df[!file.exists(data$df$full_path),]
    # Update processed docs
    data$processed <- data$df[file.exists(data$df$full_path),]
  } else {
    output$error <- renderText({""})
    saveScan()
    saveResponses()
    # Update missing docs
    data$missing <- data$df[!file.exists(data$df$full_path),]
    # Update processed docs
    data$processed <- data$df[file.exists(data$df$full_path),]
  }
  
  # Add responses to metadata if all files for the writer have been processed
  if (nrow(data$missing) == 0){
    addMetadata()
  }
})


# QR CODE -----------------------------------------------------------------
#HELPER FUNCTION: split qr code
splitQR <- function(qr_code){
  # split qr string
  qr_split <- unlist(stringr::str_split(qr_code, "/"))
  
  # grab doc type (surveys, writing, or signatures) and writer
  values$doc_type = qr_split[1]
  qr$writer = qr_split[2]
  
  # grab additional survey info. qr string format: surveys/w0001/survey1
  if (values$doc_type == "surveys"){
    # change to singular
    values$doc_type <- "survey"
    # grab session
    qr$session <- as.integer(gsub(".*?([0-9]+).*", "\\1", qr_split[3]))
  }
  
  # graph additional writer info. qr string format: writing/w0001/s01/pWOZ_r1
  if (values$doc_type == "writing"){ 
    # grab session number
    qr$session = as.numeric(gsub(".*?([0-9]+).*", "\\1", qr_split[3]))
    # split prompt and repetition
    prompt_rep = unlist(stringr::str_split(qr_split[4], "_"))
    # grab prompt. drop the "p"
    qr$prompt = stringr::str_replace(prompt_rep[1], "p","")
    # grab the repetition number
    qr$repetition = as.numeric(gsub(".*?([0-9]+).*", "\\1", prompt_rep[2]))
  }
  
  # grab addition signatures info. qr string format: signatures/w0001/JE
  if (values$doc_type == "signatures"){
    # change to singular
    values$doc_type <- "signature"
    # grab initials
    qr$initials = qr_split[3]
  }
}

#HELPER FUNCTION: format doc names
makeDocNames <- function(){
  # use qr code info to format file names
  
  # format survey
  if (values$doc_type == "survey"){
    # scan
    qr$scan_name <- paste0(qr$writer,"_survey",qr$session, ".png")
    qr$scan_path <- file.path(values$main_dir, "Stage3_Survey_Data", "Sorted", qr$writer, qr$scan_name)
    
    # crop
    qr$crop_name <- NULL
    qr$crop_path <- NULL
    
    # csv
    qr$csv_name <- paste0(qr$writer, "_survey", qr$session, ".csv")
    qr$csv_path <- file.path(values$main_dir, "Stage3_Survey_Data", "Spreadsheets", qr$writer, qr$csv_name)
  } 
  
  # format writing
  if (values$doc_type == "writing"){
    # scan
    session <- stringr::str_pad(qr$session, width = 2, side = "left", pad = 0)
    repetition <- stringr::str_pad(qr$repetition, width = 2, side = "left", pad = 0)
    qr$scan_name <- paste0(qr$writer,"_s", session, "_p", qr$prompt, "_r", repetition, "_scan.png")
    qr$scan_path <- file.path(values$main_dir, "Stage2_Sorted", "Writing", qr$writer, qr$scan_name)
    
    # cropped
    qr$crop_name <- paste0(qr$writer,"_s", session, "_p", qr$prompt, "_r", repetition, ".png")
    qr$crop_path <- file.path(values$main_dir, "Stage4_Cropped", "Writing", qr$writer, qr$crop_name)
  
    # csv
    qr$csv_name <- NULL
    qr$csv_path <- NULL
  }
  
  # format signature scan
  if (values$doc_type == "signature"){
    # scan
    qr$scan_name <- paste0(qr$writer,"_", qr$initials, "_scan.png")
    qr$scan_path <- file.path(values$main_dir, "Stage2_Sorted", "Signatures", qr$writer, qr$scan_name)
    
    # cropped
    qr$crop_name <- paste0(qr$writer,"_", qr$initials, ".png")
    qr$crop_path <- file.path(values$main_dir, "Stage4_Cropped", "Signatures", qr$writer, qr$crop_name)
    
    # csv
    qr$csv_name <- NULL
    qr$csv_path <- NULL
  }
}

#RENDER: document name and dimensions
output$upload_path <- renderText({paste0("Upload path: ", values$upload_path)})
output$current_path <- renderText({paste0("Current path: ", values$current_path)})
output$image_name <- renderText({paste0("Name: ", values$image_name)})
output$dimensions <- renderText({paste0("Dimensions: ", values$dimensions)})

#RENDER: qr code info
output$qr <- renderText({paste0("QR Code: ", qr$code)})
output$doc_type <- renderText({paste0("Document Type: ", values$doc_type)})
output$writer <- renderText({paste0("Writer: ", qr$writer)})
output$session <- renderText({paste0("Session: ", qr$session)})
output$prompt <- renderText({paste0("Prompt: ", qr$prompt)})
output$repetition <- renderText({paste0("Repetition: ", qr$repetition)})
output$initials <- renderText({paste0("Initials: ", qr$initials)})

#BUTTON: select qr code
observeEvent(input$select_qr, {
  
  if(is.null(input$preprocess_plot_brush)){
    output$error <- renderText({"Please manually select the QR code."})
  }else{ 
    output$error <- renderText({""})
    
    xmin = values$session_inv_scale*input$preprocess_plot_brush$xmin
    xmax = values$session_inv_scale*input$preprocess_plot_brush$xmax
    ymin = values$session_inv_scale*input$preprocess_plot_brush$ymin
    ymax = values$session_inv_scale*input$preprocess_plot_brush$ymax
    
    xrange = xmax - xmin
    yrange = ymax - ymin
    
    # crop qr code
    if(!is.null(xrange) && !is.null(yrange)){
      qr$image = image_crop(values$image, paste(xrange,'x', yrange, '+', xmin, '+', ymin))
    }
    
    # write qr code to temp file
    image_write(qr$image, file.path("images", "temp", "tmp_qr.png"))
    qr$qr_path <- file.path("images", "temp", "tmp_qr.png")
    
    # read qr code from temp file
    qr$code <- quadrangle::qr_scan(qr$qr_path)$values$value  # read qr code
    # extract writer, session, etc. if qr code isn't empty
    if (length(qr$code) != 0){
      splitQR(qr$code)
      # format document names
      makeDocNames()
      # enable save scan button
      shinyjs::enable("save_scan")
      # get list of docs for current writer
      data$df <- listAllDocs()
      # Update missing docs
      data$missing <- data$df[!file.exists(data$df$full_path),]
      # Update processed docs
      data$processed <- data$df[file.exists(data$df$full_path),]
    }
  }})


# Rotation ----------------------------------------------------------------
#BUTTON: rotate left
observeEvent(input$left, {
  output$error <- renderText({""})
  updateSliderInput(session, "rotation", value = input$rotation - 1)
})

#BUTTON: rotate right
observeEvent(input$right, {
  output$error <- renderText({""})
  updateSliderInput(session, "rotation", value = input$rotation + 1)
})

#BUTTON: reset crop
observeEvent(input$reset_crop, {
  output$error <- renderText({""})
  values$image <- values$uploaded_image
  values$crop_list <- list(values$image)
  
  #Reset dimensions
  values$info <- image_info(values$image)
  values$dimensions <- paste0(values$info$width, 'x', values$info$height)
  shinyjs::disable("reset_crop"); shinyjs::disable("undo_crop")
  values$current_path <- values$upload_path
  
  #Reset session values
  values$session_width = session$clientData$output_preprocess_plot_width
  values$session_scale = values$session_width / values$info$width
  values$session_inv_scale = values$info$width / values$session_width
})


# Cropping ----------------------------------------------------------------
#HELPER FUNCTION: saveCrop
saveCrop <- function(){
  # make writer folder for cropped document
  if (!dir.exists(dirname(qr$crop_path))){
    dir.create(dirname(qr$crop_path))
  }
  
  # save cropped document
  values$image %>% 
    image_rotate(input$rotation) %>% 
    image_write(path=qr$crop_path, format = 'png')
}

#BUTTON: undo crop
observeEvent(input$undo_crop, {
  output$error <- renderText({""})
  values$image <- tail(values$crop_list, 2)[[1]]
  values$crop_list <- head(values$crop_list, -1)
  
  #Reset dimensions
  values$info <- image_info(values$image)
  values$dimensions <- paste0(values$info$width, 'x', values$info$height)
  if(length(values$crop_list) == 1){
    shinyjs::disable("reset_crop"); shinyjs::disable("undo_crop")
  }
  
  #Reset session values
  values$session_width = session$clientData$output_preprocess_plot_width
  values$session_scale = values$session_width / values$info$width
  values$session_inv_scale = values$info$width / values$session_width
  
  # change to previous image
  image_write(values$image, file.path("images", "temp", "tmp.png")); values$current_path <- file.path("images", "temp", "tmp.png")
})

#BUTTON: crop
observeEvent(input$crop, {
  
  if(is.null(input$preprocess_plot_brush)){
    output$error <- renderText({"Please select an area prior to cropping."})
  } else { 
    output$error <- renderText({""})
    
    # Image scaled to fit to window when image rendered. Multiply by inverse
    # scale factor to select box on full-size image
    xmin = values$session_inv_scale*input$preprocess_plot_brush$xmin
    xmax = values$session_inv_scale*input$preprocess_plot_brush$xmax
    ymin = values$session_inv_scale*input$preprocess_plot_brush$ymin
    ymax = values$session_inv_scale*input$preprocess_plot_brush$ymax
    
    xrange = xmax - xmin
    yrange = ymax - ymin
    
    if(!is.null(xrange)){
      values$image = image_crop(values$image, paste(xrange,'x', yrange, '+', xmin, '+', ymin))
    }
    
    values$info <- image_info(values$image)
    info <- image_info(values$image)
    values$dimensions <- paste0(info$width, 'x', info$height)
    
    values$crop_list <- append(values$crop_list, values$image)
    message(paste0('crop_list:', values$crop_list, '\n'))
    
    shinyjs::enable("reset_crop"); shinyjs::enable("undo_crop")
    image_write(values$image, file.path("images", "temp", "tmp.png")); values$current_path <- file.path("images", "temp", "tmp.png")
  }})

#SAVE: scan and crop
observeEvent(input$save_docs, {
  # Return error if cropped document already exists. Otherwise, save the cropped document.
  if (file.exists(qr$scan_path) && file.exists(qr$crop_path)){
    output$error <- renderText({paste0("Scan and cropped documents already exist. Manually delete files if you want to save a updated versions.")})
  } else if (!file.exists(qr$scan_path) && file.exists(qr$crop_path)) { 
    output$error <- renderText({"Cropped document already exists. Manually delete file if you want to save an updated version."})
    saveScan()
    # Update missing docs
    data$missing <- data$df[!file.exists(data$df$full_path),]
    # Update processed docs
    data$processed <- data$df[file.exists(data$df$full_path),]
  } else if (file.exists(qr$scan_path) && !file.exists(qr$crop_path)){
    output$error <- renderText({"Scan already exists. Manually delete file if you want to save an updated version."})
    saveCrop()
    # Update missing docs
    data$missing <- data$df[!file.exists(data$df$full_path),]
    # Update processed docs
    data$processed <- data$df[file.exists(data$df$full_path),]
  } else {
    output$error <- renderText({""})
    saveScan()
    saveCrop()
    # Update missing docs
    data$missing <- data$df[!file.exists(data$df$full_path),]
    # Update processed docs
    data$processed <- data$df[file.exists(data$df$full_path),]
  }
  
  # Add responses to metadata if all files for the writer have been processed
  if (nrow(data$missing) == 0){
    addMetadata()
  }
})

# Masking -----------------------------------------------------------------
#BUTTON: reset mask
observeEvent(input$reset_mask, {
  if(nrow(values$mask_list_df) == 0){
    output$error <- renderText({"No mask to remove"})
  }else{
    output$error <- renderText({""})
    values$mask_list_df <- values$mask_list_df[0,]
    shinyjs::disable("save_mask"); shinyjs::disable("undo_mask"); shinyjs::disable("reset_mask")
  }
})

#BUTTON: undo mask
observeEvent(input$undo_mask, {
  if(nrow(values$mask_list_df) == 0){
    output$error <- renderText({"No mask to undo"})
  }else{
    output$error <- renderText({""})
    values$mask_list_df <- head(values$mask_list_df,-1)
    
    #Disable Mask
    if(nrow(values$mask_list_df) == 0){
      shinyjs::disable("save_mask"); shinyjs::disable("undo_mask"); shinyjs::disable("reset_mask")
    }
  }})

#BUTTON: mask
#Adds mask coordinates to mask_list
observeEvent(input$mask, {
  if(is.null(input$preprocess_plot_brush)){
    output$error <- renderText({"Please select an area prior to masking."})
    
  }else{
    output$error <- renderText({""})
    
    xmin = input$preprocess_plot_brush$xmin
    xmax = input$preprocess_plot_brush$xmax
    ymin = input$preprocess_plot_brush$ymin
    ymax = input$preprocess_plot_brush$ymax
    
    xrange = xmax - xmin
    yrange = ymax - ymin
    
    values$mask_list_df[nrow(values$mask_list_df) + 1,] = c(xmin, xmax, ymin, ymax, xrange, yrange)
    shinyjs::enable("save_mask"); shinyjs::enable("undo_mask"); shinyjs::enable("reset_mask")
    
    message(values$mask_list_df)
  }})

#RENDER: image with mask
output$preprocess_plot_masked <- renderImage({
  tmp = values$image
  if(nrow(values$mask_list_df) == 0){
    #output$error <- renderText({"No mask to plot"})
    
  }else{
    #do a loop through all the masks and add to image
    for (i in 1:nrow(values$mask_list_df)) {
      tmp = image_composite(
        tmp, 
        image_blank(values$mask_list_df[i, "xrange"], values$mask_list_df[i, "yrange"],  color="#ffffff80"), 
        operator = "atop", compose_args="70", 
        offset = paste0("+", values$mask_list_df[i,"xmin"], "+", values$mask_list_df[i, "ymin"])
      )
    }
  }
  
  tmp <- tmp %>%
    image_rotate(input$rotation) %>%
    image_resize(input$size) %>%
    image_write(tempfile(fileext='png'), format = 'png')
  
  # Return a list
  list(src = tmp, contentType = "image/png")
}, deleteFile = FALSE)

#SAVE: mask
output$save_mask <- downloadHandler(
  filename <- function(){
    paste("image_masked.RData")
  },
  content = function(file) {
    if(nrow(values$mask_list_df) == 0){
      output$error <- renderText({"There is no mask to save"})
    }else{
      
      #Create the mask matrix: 0 = unmasked, 1 = masked
      info <- image_info(values$image)
      mask <- matrix(0, info$height, info$width)
      
      for (i in 1:nrow(values$mask_list_df)) {
        xmin = round(values$mask_list_df[i, "xmin"])
        ymin = round(values$mask_list_df[i, "ymin"])
        xmax = round(values$mask_list_df[i, "xmax"])
        ymax = round(values$mask_list_df[i, "ymax"])
        
        for (n in ymin:ymax){
          for(m in xmin:xmax){
            mask[n,m] = 1
          }
        }
      }
      
      img <- values$image %>% image_rotate(input$rotation)
      magick_image <- image_data(img, 'rgba')
      
      save(magick_image, mask, file = file)
    }
  }
)


# Data Checks -------------------------------------------------------------

#HELPER FUNCTION: getPromptOrders
# Make dataframe of prompt orders by for each treatment
getPromptOrders <- function(){
  treatmentA = data.frame(treatment = "A", 
                          s1_prompt_order = "LND, WOZ, PHR", 
                          s2_prompt_order = "WOZ, PHR, LND", 
                          s3_prompt_order = "PHR, LND, WOZ")
  treatmentB = data.frame(treatment = "B",
                          s1_prompt_order = "WOZ, PHR, LND", 
                          s2_prompt_order = "PHR, LND, WOZ", 
                          s3_prompt_order = "LND, WOZ, PHR")
  treatmentC = data.frame(treatment = "C",
                          s1_prompt_order = "PHR, LND, WOZ", 
                          s2_prompt_order = "LND, WOZ, PHR", 
                          s3_prompt_order = "WOZ, PHR, LND")
  treatments = rbind(treatmentA, treatmentB, treatmentC)
  return(treatments)
}

#HELPER FUNCTION: getSignatureNames
# Make a dataframe of signature names by writer ID
getSignatureNames <- function(){
  
  # WID 1-60
  df1 <- data.frame(WID_start = 1, 
                    WID_end = 60, 
                    treatment = "A", 
                    s1_name = "Edward Franco", 
                    s2_name = "Brad Harvey", 
                    s3_name = "Nolan Henson")
  # WID 61-120
  df2 <- data.frame(WID_start = 61, 
                    WID_end = 120, 
                    treatment = "B", 
                    s1_name = "Trisha Middleton", 
                    s2_name = "Alec Adams", 
                    s3_name = "Geoffrey Simon")
  # WID 121-180
  df3 <- data.frame(WID_start = 121, 
                    WID_end = 180, 
                    treatment = "C", 
                    s1_name = "Bernard Crane", 
                    s2_name = "Caroline Guerra", 
                    s3_name = "Minnie Burch")
  # WID 181-240
  df4 <- data.frame(WID_start = 181, 
                    WID_end = 240, 
                    treatment = "A", 
                    s1_name = "Jordi Howe", 
                    s2_name = "Isabella Gamble", 
                    s3_name = "Kristin Bautista")
  # WID 241-300
  df5 <- data.frame(WID_start = 241,
                    WID_end = 300, 
                    treatment = "B", 
                    s1_name = "Brenden Dyer", 
                    s2_name = "Aiden Fry", 
                    s3_name = "Milton Perkins")
  # WID 301-360
  df6 <- data.frame(WID_start = 301, 
                    WID_end = 360, 
                    treatment = "C", 
                    s1_name = "Diane Carson", 
                    s2_name = "Tommy Case", 
                    s3_name = "Zoey Mills")
  # WID 361-420
  df7 <- data.frame(WID_start = 361, 
                    WID_end = 420, 
                    treatment = "A", 
                    s1_name = "Ivan Brewer", 
                    s2_name = "Natasha Woods", 
                    s3_name = "Herman Shaw")
  # WID 421-480
  df8 <- data.frame(WID_start = 421, 
                    WID_end = 480, 
                    treatment = "B", 
                    s1_name = "Nettie Brooks", 
                    s2_name = "Lawrence Richards", 
                    s3_name = "Cecelia Franklin")
  # WID 481-540
  df9 <- data.frame(WID_start = 481, 
                    WID_end = 540, 
                    treatment = "C", 
                    s1_name = "Paul Conner", 
                    s2_name = "Elsa Medina", 
                    s3_name = "David Gill")
  # WID 541-600
  df10 <- data.frame(WID_start = 541, 
                     WID_end = 600, 
                     treatment = "A", 
                     s1_name = "Samuel Scott", 
                     s2_name = "Karen Turner", 
                     s3_name = "Aurthur Bell")
  # WID 601-660
  df11 <- data.frame(WID_start = 601, 
                     WID_end = 660, 
                     treatment = "B", 
                     s1_name = "Judy Evans", 
                     s2_name = "Terry Harris", 
                     s3_name = "Martha Harris")
  # WID 661-720
  df12 <- data.frame(WID_start = 661, 
                     WID_end = 720, 
                     treatment = "C", 
                     s1_name = "Stephen James", 
                     s2_name = "Rosa Santiago", 
                     s3_name = "William Green")
  
  # Master dataframe
  df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
  
  # Get initials for session 1
  initials1 <- stringr::str_extract_all(df$s1_name, "[A-Z]+")  # extract capital letters from names
  initials1 <- unlist(lapply(initials1, function(x) paste0(x[1], x[2])))  # concatenate letters
  df['s1_initials'] <- initials1
  
  # Get initials for session 2
  initials2 <- stringr::str_extract_all(df$s2_name, "[A-Z]+")  # extract capital letters from names
  initials2 <- unlist(lapply(initials2, function(x) paste0(x[1], x[2])))  # concatenate letters
  df['s2_initials'] <- initials2
  
  # Get initials for session 3
  initials3 <- stringr::str_extract_all(df$s3_name, "[A-Z]+")  # extract capital letters from names
  initials3 <- unlist(lapply(initials3, function(x) paste0(x[1], x[2])))  # concatenate letters
  df['s3_initials'] <- initials3
  
  return(df)
}

#HELPER FUNCTION: lookupInitials
# Lookup the signature initials based on writer id 
lookupInitials <- function(writer){
  
  id <- stringr::str_extract(writer, "\\d+")
  id <- as.integer(id)
  
  df <- getSignatureNames()
  
  # filter by writer id #
  df <- df[df$WID_start <= id & df$WID_end >= id,]
  
  # make vector of initials
  initials <- c(df$s1_initials, df$s2_initials, df$s3_initials)
  return(initials)
}

#HELPER FUNCTION: listAllDocs
# Make a dataframe of all document names and file paths for the current writer
listAllDocs <- function(){
  # create a list of every document that a writer should have
  
  # survey csv files
  survey_csvs <- file.path(values$main_dir, "Stage3_Survey_Data", "Spreadsheets", qr$writer, paste0(qr$writer, "_survey", 1:3, ".csv"))
  
  # survey scan files
  survey_scans <- file.path(values$main_dir, "Stage3_Survey_Data", "Sorted", qr$writer, paste0(qr$writer, "_survey", 1:3, ".png"))
  
  # get signature initials
  initials <- lookupInitials(writer = qr$writer)
  
  # signature scans 
  sig_scans <- file.path(values$main_dir, "Stage2_Sorted", "Signatures", qr$writer, paste0(qr$writer, "_", initials, "_scan.png"))
  
  # signature crops
  sig_crops <- file.path(values$main_dir, "Stage4_Cropped", "Signatures", qr$writer, paste0(qr$writer, "_", initials, ".png"))
  
  # writing scans and crops
  writing_scans <- writing_crops <- c()
  s = c("_s01", "_s02", "_s03")
  p = c("_pLND", "_pWOZ", "_pPHR")
  r = c("_r01", "_r02", "_r03")
  for (i in 1:3){
    for (j in 1:3){
      for (k in 1:3){
        temp_scan <- file.path(values$main_dir, "Stage2_Sorted", "Writing", qr$writer, paste0(qr$writer, s[i], p[j], r[k], "_scan.png"))
        temp_crop <- file.path(values$main_dir, "Stage4_Cropped", "Writing", qr$writer, paste0(qr$writer, s[i], p[j], r[k], ".png"))
        writing_scans <- c(writing_scans, temp_scan)
        writing_crops <- c(writing_crops, temp_crop)
      }
    }
  }
  
  df = data.frame("full_path" = survey_csvs, "doc_type" = "survey")
  df = rbind(df, data.frame("full_path" = survey_scans, "doc_type" = "survey"))
  df = rbind(df, data.frame("full_path" = sig_scans, "doc_type" = "signature"))
  df = rbind(df, data.frame("full_path" = sig_crops, "doc_type" = "signature"))
  df = rbind(df, data.frame("full_path" = writing_scans, "doc_type" = "writing"))
  df = rbind(df, data.frame("full_path" = writing_crops, "doc_type" = "writing"))
  df['file'] = basename(df$full_path)
  
  return(df)
}

#BUTTON: refresh data check tabls
observeEvent(input$refresh, {
  # Update missing docs
  data$missing <- data$df[!file.exists(data$df$full_path),]
  # Update processed docs
  data$processed <- data$df[file.exists(data$df$full_path),]
})

#RENDER: missing documents
output$docs_missing <- renderDT({
  # Select columns
  data$missing[,c("doc_type", "file")]
})

#RENDER processed documents
output$docs_processed <- renderDT({
  # Select columns
  data$processed[,c("doc_type", "file")]
})


# Testing -----------------------------------------------------------------
output$csv_path <- renderText({qr$csv_path})
output$scan_name <- renderText({paste0("Scan name: ", qr$scan_name)})
output$scan_path <- renderText({paste0("Scan path: ", qr$scan_path)})
output$crop_name <- renderText({paste0("Crop name: ", qr$crop_name)})
output$crop_path <- renderText({paste0("Crop path: ", qr$crop_path)})
