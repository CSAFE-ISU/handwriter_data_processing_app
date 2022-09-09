#=================== PREPROCESSING =======================


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
  
  # reset qr code reactive values
  values$doc_type <- "default"
  values$writer <- values$session <- values$prompt <- values$repetition <- values$initials <- values$scan_name <- values$scan_path <- values$crop_name <- values$crop_path <- values$qr_path <- values$doc <- NULL
  
  # reset survey responses input boxes
  updateTextInput(session, "response_initials", value = "")
  updateTextInput(session, "response_location", value = "")
  updateRadioButtons(session, "response_time", selected = "a. Early morning (earlier than 9:30am)")
  updateDateInput(session, "response_date", value = NA)
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
  
  # read QR code and get document info
  values$qr <- quadrangle::qr_scan(values$image)$values$value  # read qr code
  # if qr code isn't empty, format doc names
  if (length(values$qr) != 0){
    # get info from qr code
    splitQR(values$qr)
    # format document names
    makeDocNames()
    # enable save scan button
    shinyjs::enable("save_scan")
    # get list of docs for current writer
    data$df <- listAllDocs()
  }
  
  # extract number from writer id for survey response table
  if (values$doc_type == 'survey'){
    id <- stringr::str_extract(values$writer, "\\d+")
    survey$df['WID'] <- as.integer(id)
  }
  
  # update current document info
  values$image_name <- input$upload$name

  # clean up
  values$crop_list <- list(values$image)
  values$mask_list_df <- values$mask_list_df[0,]  # keep column names, clear all rows
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
#SAVE: scan
observeEvent(input$save_scan, {
  # Return error if scan already exists. Otherwise, save the scan.
  if(file.exists(values$scan_path)){
    output$error <- renderText({paste0("Scan already exists: ", values$scan_path, "\n Manually delete scan if you want to save an updated version.")})
  }else{ 
    output$error <- renderText({""})
    
    # make writer folder for scan
    if (!dir.exists(dirname(values$scan_path))){
      dir.create(dirname(values$scan_path))
    }
    
    # save original scan
    values$uploaded_image %>% 
      image_rotate(input$rotation) %>% 
      image_write(path=values$scan_path, format = 'png')
  }
})


# Survey -----------------------------------------------------------------
#CREATE: survey reactive values
survey <- reactiveValues(
  # all surveys
  df = data.frame(
    "WID" = "",
    "Initials" = "",
    "Location" = "",
    "Time" = "",
    "Date" = ""),
  # survey1 only
  df1 = data.frame(
    "ThirdGradeLoc" = "",
    "Age" = "",
    "Language" = "",
    "Gender" = "",
    "Other" = "",
    "Ethnicity" = "",
    "Edu" = "",
    "Hand" = ""
  )
)

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
  # Return error if survey csv already exists.
  if(file.exists(survey$csv_path)){
    output$error <- renderText({paste0("CSV already exists: ", survey$csv_path, "\n Manually delete CSV if you want to save an updated version.")})
    return()
  }
  
  output$error <- renderText({""})
  
  # make writer folder for csv
  if (!dir.exists(dirname(survey$csv_path))){
    dir.create(dirname(survey$csv_path))
  }
  
  if (values$session == 1){
    # combine dataframes
    df = cbind(survey$df, survey$df1)
    
    # sort columns to match previous csv files
    sorted = df[c("WID", "Initials", "Location", "Date", "Time", "ThirdGradeLoc",
                  "Age", "Language", "Gender", "Other", "Ethnicity", "Edu",
                  "Hand")]
    
    # save csv
    write.csv(sorted, file = survey$csv_path)
  } else {
    df = survey$df
    
    # sort columns to match previous csv files
    sorted = df[c("WID", "Initials", "Location", "Date", "Time")]
    
    # rename column to match previous csv files
    colnames(sorted)[colnames(sorted) == "Location"] <- "Current_Location"
    
    # save csv
    write.csv(sorted, file = survey$csv_path)
  }
})


# QR CODE -----------------------------------------------------------------
#HELPER FUNCTION: split qr code
splitQR <- function(qr){
  # split qr string
  qr_split <- unlist(stringr::str_split(qr, "/"))
  
  # grab doc type (surveys, writing, or signatures) and writer
  values$doc_type = qr_split[1]
  values$writer = qr_split[2]
  
  # grab additional survey info. qr string format: surveys/w0001/survey1
  if (values$doc_type == "surveys"){
    # change to singular
    values$doc_type <- "survey"
    # grab session
    values$session <- as.numeric(gsub(".*?([0-9]+).*", "\\1", qr_split[3]))
  }
  
  # graph additional writer info. qr string format: writing/w0001/s01/pWOZ_r1
  if (values$doc_type == "writing"){ 
    # grab session number
    values$session = as.numeric(gsub(".*?([0-9]+).*", "\\1", qr_split[3]))
    # split prompt and repetition
    prompt_rep = unlist(stringr::str_split(qr_split[4], "_"))
    # grab prompt. drop the "p"
    values$prompt = stringr::str_replace(prompt_rep[1], "p","")
    # grab the repetition number
    values$repetition = as.numeric(gsub(".*?([0-9]+).*", "\\1", prompt_rep[2]))
  }
  
  # grab addition signatures info. qr string format: signatures/w0001/JE
  if (values$doc_type == "signatures"){
    # change to singular
    values$doc_type <- "signature"
    # grab initials
    values$initials = qr_split[3]
  }
}

#HELPER FUNCTION: format doc names
makeDocNames <- function(){
  # use qr code info to format file names
  
  # format survey
  if (values$doc_type == "survey"){
    # scan
    values$scan_name <- paste0(values$writer,"_survey",values$session, ".png")
    values$scan_path <- file.path(values$main_dir, "Stage3_Survey_Data", "Sorted", values$writer, values$scan_name)
    
    # crop
    values$crop_name <- NULL
    values$crop_path <- NULL
    
    # csv
    survey$csv_name <- paste0(values$writer, "_survey", values$session, ".csv")
    survey$csv_path <- file.path(values$main_dir, "Stage3_Survey_Data", "Spreadsheets", values$writer, survey$csv_name)
  } 
  
  # format writing
  if (values$doc_type == "writing"){
    # scan
    session <- stringr::str_pad(values$session, width = 2, side = "left", pad = 0)
    repetition <- stringr::str_pad(values$repetition, width = 2, side = "left", pad = 0)
    values$scan_name <- paste0(values$writer,"_s", session, "_p", values$prompt, "_r", repetition, "_scan.png")
    values$scan_path <- file.path(values$main_dir, "Stage2_Sorted", "Writing", values$writer, values$scan_name)
    
    # cropped
    values$crop_name <- paste0(values$writer,"_s", session, "_p", values$prompt, "_r", repetition, ".png")
    values$crop_path <- file.path(values$main_dir, "Stage4_Cropped", "Writing", values$writer, values$crop_name)
  
    # csv
    survey$csv_name <- NULL
    survey$csv_path <- NULL
  }
  
  # format signature scan
  if (values$doc_type == "signature"){
    # scan
    values$scan_name <- paste0(values$writer,"_", values$initials, "_scan.png")
    values$scan_path <- file.path(values$main_dir, "Stage2_Sorted", "Signatures", values$writer, values$scan_name)
    
    # cropped
    values$crop_name <- paste0(values$writer,"_", values$initials, ".png")
    values$crop_path <- file.path(values$main_dir, "Stage4_Cropped", "Signatures", values$writer, values$crop_name)
    
    # csv
    survey$csv_name <- NULL
    survey$csv_path <- NULL
  }
}

#RENDER: document name and dimensions
output$upload_path <- renderText({paste0("Upload path: ", values$upload_path)})
output$current_path <- renderText({paste0("Current path: ", values$current_path)})
output$image_name <- renderText({paste0("Name: ", values$image_name)})
output$dimensions <- renderText({paste0("Dimensions: ", values$dimensions)})

#RENDER: qr code info
output$qr <- renderText({paste0("QR Code: ", values$qr)})
output$doc_type <- renderText({paste0("Document Type: ", values$doc_type)})
output$writer <- renderText({paste0("Writer: ", values$writer)})
output$session <- renderText({paste0("Session: ", values$session)})
output$prompt <- renderText({paste0("Prompt: ", values$prompt)})
output$repetition <- renderText({paste0("Repetition: ", values$repetition)})
output$initials <- renderText({paste0("Initials: ", values$initials)})

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
      values$qr_image = image_crop(values$image, paste(xrange,'x', yrange, '+', xmin, '+', ymin))
    }
    
    # write qr code to temp file
    image_write(values$qr_image, file.path("images", "temp", "tmp_qr.png"))
    values$qr_path <- file.path("images", "temp", "tmp_qr.png")
    
    # read qr code from temp file
    values$qr <- quadrangle::qr_scan(values$qr_path)$values$value  # read qr code
    # extract writer, session, etc. if qr code isn't empty
    if (length(values$qr) != 0){
      splitQR(values$qr)
      # format document names
      makeDocNames()
      # enable save scan button
      shinyjs::enable("save_scan")
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
  }else{ 
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

#SAVE: crop
observeEvent(input$save_crop, {
  # Return error if cropped document already exists. Otherwise, save the cropped document.
  if(file.exists(values$crop_path)){
    output$error <- renderText({paste0("Cropped document already exists: ", values$crop_path, "\n Manually delete scan if you want to save an updated version.")})
  }else{ 
    output$error <- renderText({""})
    
    # make writer folder for cropped document
    if (!dir.exists(dirname(values$crop_path))){
      dir.create(dirname(values$crop_path))
    }
    
    # save original cropped document
    values$image %>% 
      image_rotate(input$rotation) %>% 
      image_write(path=values$crop_path, format = 'png')
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
listAllDocs <- function(){
  # create a list of every document that a writer should have
  
  # survey csv files
  survey_csvs <- file.path(values$main_dir, "Stage3_Survey_Data", "Spreadsheets", values$writer, paste0(values$writer, "_survey", 1:3, ".csv"))
  
  # survey scan files
  survey_scans <- file.path(values$main_dir, "Stage3_Survey_Data", "Sorted", values$writer, paste0(values$writer, "_survey", 1:3, ".png"))
  
  # signature scans 
  sig_scans <- file.path(values$main_dir, "Stage2_Sorted", "Signatures", values$writer, paste0(values$writer, 1:3, "_scan.png"))
  
  # signature crops
  sig_crops <- file.path(values$main_dir, "Stage4_Cropped", "Signatures", values$writer, paste0(values$writer, 1:3, ".png"))
  
  # writing scans and crops
  writing_scans <- writing_crops <- c()
  s = c("_s01", "_s02", "_s03")
  p = c("_pLND", "_pWOZ", "_pPHR")
  r = c("_r01", "_r02", "_r03")
  for (i in 1:3){
    for (j in 1:3){
      for (k in 1:3){
        temp_scan <- file.path(values$main_dir, "Stage2_Sorted", "Writing", values$writer, paste0(values$writer, s[i], p[j], r[k], "_scan.png"))
        temp_crop <- file.path(values$main_dir, "Stage4_Cropped", "Writing", values$writer, paste0(values$writer, s[i], p[j], r[k], ".png"))
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

output$docs_missing <- renderDataTable({
  # Filter for missing docs
  missing <- data$df[!file.exists(data$df$full_path),]
  # Select columns
  missing[,c("doc_type", "file")]
  })

output$docs_processed <- renderDataTable({
  # Filter for processed docs
  processed <- data$df[file.exists(data$df$full_path),]
  # Select columns
  processed[,c("doc_type", "file")]
})

# Testing -----------------------------------------------------------------
output$csv_path <- renderText({survey$csv_path})
output$scan_name <- renderText({paste0("Scan name: ", values$scan_name)})
output$scan_path <- renderText({paste0("Scan path: ", values$scan_path)})
output$crop_name <- renderText({paste0("Crop name: ", values$crop_name)})
output$crop_path <- renderText({paste0("Crop path: ", values$crop_path)})
