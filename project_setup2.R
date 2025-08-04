#-------------------------------------------------------------------------------
# PROJECT SETUP FUNCTION -------------------------------------------------------
# ------------------------------------------------------------------------------


project_setup <- function(
    # Name to give to the overall project
    project_name = "",
    # MANUSCRIPT SETUP OPTIONS
    manuscript = TRUE, # possibility of disabling manuscript qmd creation
    author = NULL,  # Name of author
    title = NULL,   # Working title
    subtitle = NULL,# Working Subtitle
    references = TRUE, # create empty bibtex file  
    title_page = FALSE, # have dedicated title page
    # PRESENTATION SETUP OPTIONS
    presentation = TRUE, # create presentation qmd
    uma_style = TRUE,    # uni MA style for presentation qmd
    title_image_path = "./images/uma_palace.png",
    logo_path = "./images/uma_ss.png",
    # Other logistics
    code_files = TRUE,   # create code documentation qmd files
    data_folders = TRUE, # create data folders
    gitignore = TRUE,    # create gitignore file
    overwrite = TRUE     # overwrite existing folders and files
    ) {
  
  
  
  
  ### --------------------------------------------------------------------------
  ### 1. Input Validation and Argument Checks
  ### --------------------------------------------------------------------------

  
  # Ensure a project name is provided
  if (project_name == "") {
    stop("You must provide a 'project_name'.")
  }
  
  # Ensure project name is character
  if (! is.character(project_name)) {
    stop("No character value provided to 'project_name'.")
  }
  
  
  
  
  
  
  
  
  
  
}