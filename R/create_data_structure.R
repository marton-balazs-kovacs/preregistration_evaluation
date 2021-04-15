#' Function to create a data folder structure locally
#' 
#' The function creates a local folder structure to store the data in the
#' proper format.
#' 
#' @param pth path to the folder where to store the data as a subfolder
create_data_structure <- function(pth) {
  verbose_create <- function(f) {
    if(!dir.exists(f)) {
      message(sprintf("Directory %s does not exist, creating..", f))
      dir.create(f, recursive = T)  
    } else {
      message(sprintf("Directory %s already exist.", f))
    }
  }
  
  main_dir <- file.path(pth, "Main")
  source_dir <- file.path(main_dir, "Source")
  raw_dir <- file.path(main_dir, "Raw")
  processed_dir <- file.path(main_dir, "Processed")
  
  verbose_create(main_dir)
  verbose_create(source_dir)
  verbose_create(raw_dir)
  verbose_create(processed_dir)
}
