server <- function(input, output, session) {
  
  #Tooltips
  source(file.path("server", "tooltips.R"), local = TRUE)$value
  
  #========================================================
  #=================== GLOBAL SET UP ======================
  #========================================================
  
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
  values$qr <- NULL
  
  # main directory based on OS (Darwin = Mac)
  switch(Sys.info()[['sysname']],
         Windows = {values$main_dir = "/lss/research/csafe-handwriting-irb/Data_Processing_App_Testing"},
         Darwin = {values$main_dir = "/Volumes/lss/research/csafe-handwriting-irb/Data_Processing_App_Testing"})
  
  # create subdirectory temp in images 
  if (!dir.exists(file.path("images", "temp"))){
    dir.create(file.path("images", "temp"))
  }
  
  #========================================================
  #================ SOURCE SERVER CODE ====================
  #========================================================
  
  source(file.path("server", "tooltips.R"), local = TRUE)$value
  source(file.path("server", "preProcess.R"), local = TRUE)$value
  source(file.path("server", "dataIntegrityChecks.R"), local = TRUE)$value
  
}
