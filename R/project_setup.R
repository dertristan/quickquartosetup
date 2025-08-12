#' Project Setup
#'
#' Initializes a new R project with a standardized folder structure and optional files
#' for manuscripts, presentations, code, and data management.
#'
#' @param project_name A character string specifying the project name. This will be used to name the main project directory. Defaults to an empty string (`""`).
#' @param target_path A character string specifying the path where the project should be created. You can specify a custom path; otherwise, the current working directory is used. Defaults to `"."`.
#'
#' @param manuscript Logical. If `TRUE`, creates a Quarto manuscript file (`manuscript.qmd`). Defaults to `TRUE`.
#' @param author A character string specifying the author's name. Included in manuscript, presentation, and code files. Defaults to `NULL`.
#' @param institution A character string specifying the author's institution. Included in `manuscript.qmd`. Defaults to `NULL`.
#' @param mail A character string specifying the author's email address. Included in `manuscript.qmd`. Defaults to `NULL`.
#' @param student_id A character string specifying the student's ID. Included in `manuscript.qmd`. Defaults to `NULL`.
#' @param title A character string specifying the working title of the project. Used in manuscript, presentation, and code files. Defaults to `NULL`.
#' @param subtitle A character string specifying the subtitle of the project. Used in presentation and code files. Defaults to `NULL`.
#' @param title_page Logical. If `TRUE`, generates a dedicated title page using the Quarto `titlepages` extension. Defaults to `FALSE`.
#' @param logo Logical. If `TRUE`, includes a logo on the manuscript. Only applies if `title_page = TRUE`. Defaults to `FALSE`.
#' @param stat_decl Logical. If `TRUE`, adds a statutory declaration (e.g., for exam papers). Defaults to `FALSE`.
#'
#' @param presentation Logical. If `TRUE`, creates a Quarto Reveal.js presentation (`presentation.qmd`). Defaults to `TRUE`.
#' @param uma_style Logical. If `TRUE`, applies a custom University of Mannheim style, including SCSS theme, logo, and title image. Defaults to `TRUE`.
#' @param title_image_path A character string specifying the path to the presentation's title image. Defaults to `"./images/uma_palace.png"`.
#' @param logo_path A character string specifying the path to the logo used in manuscript or presentation. Defaults to `"./images/uma_ss.png"`.
#'
#' @param code_files Logical. If `TRUE`, creates a code notebook file (`01_code.qmd`) for documentation. Defaults to `TRUE`.
#' @param data_folders Logical. If `TRUE`, creates standard data subfolders (`raw`, `processed`, `final`). Defaults to `TRUE`.
#' @param gitignore Logical. If `TRUE`, generates a `.gitignore` file. Defaults to `TRUE`.
#' @param overwrite Logical. If `TRUE`, allows overwriting existing files or folders with the same names. Defaults to `TRUE`.
#'
#' @export

project_setup <- function(
    project_name = "",
    target_path = ".",
    # MANUSCRIPT SETUP OPTIONS
    manuscript = TRUE,
    author = NULL,
    institution = NULL,
    mail = NULL,
    student_id = NULL,
    title = NULL,
    subtitle = NULL,
    title_page = FALSE,
    logo = FALSE,
    stat_decl = FALSE,
    # PRESENTATION SETUP OPTIONS
    presentation = TRUE,
    uma_style = TRUE,
    title_image_path = "./images/uma_palace.png",
    logo_path = "./images/uma_ss.png",
    # Other logistics
    code_files = TRUE,
    data_folders = TRUE,
    gitignore = TRUE,
    overwrite = TRUE) {
  # --------------------------------------------------------------------------
  # 1. Input Validation and Argument Checks
  # --------------------------------------------------------------------------

  # Ensure a project name is a non-empty character string.
  stopifnot(
    "You must provide a 'project_name'." = project_name != "",
    "The 'project_name' must be a character string." = is.character(project_name)
  )

  # Check for whitespace or disallowed special characters in project_name
  if (grepl("[^A-Za-z0-9_.]", project_name)) {
    warning(
      "The 'project_name' contains disallowed characters. ",
      "It is recommended to use only letters, numbers, underscores (_), or dots (.) for folder names."
    )
  }



  # Construct the full project path.
  full_project_path <- file.path(target_path, project_name)

  # Check if the project directory already exists and if overwriting is disallowed.
  if (dir.exists(full_project_path) && !overwrite) {
    stop(
      "Project directory '", full_project_path, "' already exists. ",
      "Set `overwrite = TRUE` to continue (files may be overwritten)."
    )
  }

  # Handle NULL values for metadata by providing sensible defaults.
  # Get the system username for 'author'. We check multiple environment variables
  # for cross-platform compatibility.
  if (is.null(author)) {
    author <- Sys.getenv("LOGNAME", unset = "")
    if (author == "") {
      author <- Sys.getenv("USER", unset = "")
    }
    if (author == "") {
      author <- Sys.getenv("USERNAME", unset = "Your Name Here")
    }
  }

  if (is.null(institution)) institution <- "Your Institution"
  if (is.null(mail)) mail <- "your.email@your.institution.com"
  if (is.null(student_id)) student_id <- "1234567"
  if (is.null(title)) title <- "Untitled Project"
  if (is.null(subtitle)) subtitle <- "A great project"

  # --- Construct the 'author_with_details' string for the YAML header ---
  # The goal is to build a string like: "Firstname Lastname^[Institution; Mail: email; student ID: id]"

  # Check and convert student_id to character if necessary.
  if (!is.null(student_id) && !is.character(student_id)) {
    student_id <- as.character(student_id)
  }

  # Build a vector of details strings only for non-default values.
  details <- c(
    if (!is.null(institution) && institution != "Your Institution") institution,
    if (!is.null(mail) && mail != "your.email@your.institution.com") paste0("Mail: ", mail),
    if (!is.null(student_id) && student_id != "1234567") paste0("Student ID: ", student_id)
  )

  # Combine the details into a single string, separated by semicolons.
  if (length(details) > 0) {
    author_with_details <- paste0(author, "^[", paste(details, collapse = "; "), "]")
  } else {
    # If no non-default details were provided, just use the author's name.
    author_with_details <- author
  }

  # --- Construct the 'author_with_id' variable ---
  # The goal is to build a string like: "Firstname Lastname (Student ID)"
  if (!is.null(student_id) && student_id != "1234567") {
    author_with_id <- paste0(author, " (", student_id, ")")
  } else {
    # If no non-default student ID was provided, just use the author's name.
    author_with_id <- author
  }

  # Check for the existence of template images if presentation is TRUE
  # and the UMA style is requested.
  if (presentation && uma_style) {
    # Check if each source files can be found.
    image_files_exists <- file.exists(system.file("images", package = "quickquartosetup"))


    if (!image_files_exists) {
      # If any images are missing, throw a warning and disable the UMA style.
      warning(
        "Logo or Title Image File not found for UMA style. ",
        "Presentation will still be created, but without the UMA style templates."
      )
      # Set uma_style to FALSE to prevent the function from trying to copy them later.
      uma_style <- FALSE
    }
  }

  # --------------------------------------------------------------------------
  # 2. Helper Functions (Encapsulated)
  # --------------------------------------------------------------------------

  ## Create a Folder if It Does Not Exist
  ##
  ## This helper function ensures a specified folder path exists. If the folder
  ## does not exist, it creates it recursively. It provides informative messages
  ## about the action taken.
  ##
  ## @param folder_path A character string specifying the path of the folder to create.
  ## @return Invisible `NULL`. Called for its side effects (folder creation and messages).

  create_folder <- function(folder_path) {
    if (!dir.exists(folder_path)) {
      # Create the directory recursively, meaning any necessary parent directories
      # will also be created.
      dir.create(folder_path, recursive = TRUE)
      message("  -> Created folder: '", folder_path, "' (recursively)")
    } else {
      # Inform the user if the folder already exists, no action needed.
      message("  -> Folder already exists: '", folder_path, "' (skipping creation)")
    }
    invisible(NULL) # Return invisible NULL as this function is for side effects
  }

  ## Create or Overwrite a File with Specified Content
  ##
  ## This helper function writes content to a file. It checks for the file's
  ## existence and respects the `overwrite` flag. If `overwrite` is `FALSE` and
  ## the file exists, it will skip writing.
  ##
  ## @param file_path A character string specifying the full path to the file to create.
  ## @param content A character vector (or single string) containing the content to write to the file.
  ## @param overwrite A logical value. If `TRUE`, an existing file will be overwritten.
  ##   If `FALSE` and the file exists, the function will skip writing and issue a message.
  ## @return Invisible `NULL`. Called for its side effects (file creation/writing and messages).
  create_file_with_content <- function(file_path, content, overwrite) {
    # Determine if the file already exists before attempting to write.
    file_exists_before_write <- file.exists(file_path)

    if (!file_exists_before_write || overwrite) {
      # Write the content to the file.
      # writeLines is suitable for text content, preserving line breaks.
      writeLines(content, file_path)

      # Provide a message based on whether the file was newly created or overwritten.
      if (file_exists_before_write && overwrite) {
        message("  -> Overwrote existing file: '", file_path, "'")
      } else {
        message("  -> Created file: '", file_path, "'")
      }
    } else {
      # Inform the user if the file exists and overwrite is FALSE.
      message("  -> File already exists (skipping, overwrite = FALSE): '", file_path, "'")
    }
    invisible(NULL) # Return invisible NULL as this function is for side effects
  }

  ## Copy Files or Folders to a Destination Folder
  ##
  ## This helper function copies a set of source items (files or folders) to a
  ## specified destination folder. It first ensures the destination folder exists.
  ## It handles cases where source items are missing and respects the `overwrite`
  ## flag for existing destination items.
  ##
  ## @param source_paths A character vector of full paths to the source files or
  ##   folders to be copied.
  ## @param dest_folder A character string specifying the path to the destination folder.
  ## @param overwrite A logical value. If `TRUE`, existing files/folders in the
  ##   destination with the same name will be overwritten. If `FALSE`, they will
  ##   not be copied.
  ## @return Invisible `NULL`. Called for its side effects (item copying and messages).
  copy_items <- function(source_paths, dest_folder, overwrite) {
    message("Starting copy_items function.")
    message(paste("Source paths:", paste(source_paths, collapse = ", ")))
    message(paste("Destination folder:", dest_folder))

    # Check if the destination folder exists, create if not
    if (!dir.exists(dest_folder)) {
      message(paste("Destination folder does not exist. Creating:", dest_folder))
      dir.create(dest_folder, recursive = TRUE)
    } else {
      message(paste("Destination folder already exists:", dest_folder))
    }

    # Iterate over each source path
    for (src in source_paths) {
      message(paste("Processing source item:", src))

      # Check if the source path exists
      if (!file.exists(src)) {
        warning(paste("Source item missing:", src))
        message(paste("Skipping missing item:", src))
        next
      }

      # Use file.copy with the destination folder as the 'to' argument
      message(paste("Copying from:", src, "to:", dest_folder))
      file.copy(from = src, to = dest_folder, recursive = TRUE, overwrite = overwrite)
      message(paste("Copy of", src, "complete."))
    }
    message("copy_items function finished.")
  }



  ### --------------------------------------------------------------------------
  ### 3. Project Directory Creation and Scoping
  ### --------------------------------------------------------------------------

  message("\nStarting project setup for '", project_name, "'.")
  dir.create(full_project_path, recursive = TRUE, showWarnings = FALSE)
  message("Created project directory at: '", full_project_path, "'")

  # Store the original working directory and set a return hook
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(full_project_path)


  ### --------------------------------------------------------------------------
  ### 5. Define All File Content Strings
  ### --------------------------------------------------------------------------

  source("quarto_mansucript_default.R")

  source("stat_decl.R")

  quarto_manuscript_content_default_statutory_decl <- paste0(
    quarto_manuscript_content_default,
    stat_decl_content
  )

  source("quarto_manuscript_titlepage.R")

  quarto_manuscript_content_titlepage_statutory_decl <- paste0(
    quarto_manuscript_content_titlepage,
    stat_decl_content
  )

  source("quarto_manuscript_titlepage_logo.R")

  quarto_manuscript_content_titlepage_logo_statutory_decl <- paste0(
    quarto_manuscript_content_titlepage_logo,
    stat_decl_content
  )

  source("bibtex.R")

  source("quarto_code_notebook.R")

  source("quarto_presentation_default.R")

  source("quarto_presentation_unima.R")

  source("gitignore.R")


  ### --------------------------------------------------------------------------
  ### 6. Main Logic: Folder and File Creation
  ### --------------------------------------------------------------------------

  # --- Create Core Folders ---
  if (data_folders) {
    message("\nCreating core folders...")
    core_folders <- c(
      "code",
      "data/01_raw",
      "data/02_processed",
      "data/03_final"
    )
    invisible(lapply(core_folders, create_folder))
  }


  # --- Create References File ---
  message("\nCreating bibliography file...")
  create_file_with_content(
    file_path = "references.bib",
    content = ref_bib,
    overwrite = overwrite
  )



  # --- Conditional File Creation ---

  # Manuscript files
  if (manuscript) {
    message("\nCreating manuscript files...")

    message("\nCopying Quarto extensions (Wordcount & Titlepage)")
    copy_items(
      source_paths = system.file("_extensions", package = "quickquartosetup"),
      dest_folder = getwd(),
      overwrite = overwrite
    )

    copy_items(
      source_paths = system.file("images", package = "quickquartosetup"),
      dest_folder = getwd(),
      overwrite = overwrite
    )


    if (title_page && logo && stat_decl) {
      content_tmp <- quarto_manuscript_content_titlepage_logo_statutory_decl
    }

    if (title_page && logo && !(stat_decl)) {
      content_tmp <- quarto_manuscript_content_titlepage_logo
    }

    if (title_page && stat_decl && !(logo)) {
      content_tmp <- quarto_manuscript_content_titlepage_logo
    }

    if (title_page && !(stat_decl) && !(logo)) {
      content_tmp <- quarto_manuscript_content_titlepage
    }

    if ((!title_page) && stat_decl) {
      content_tmp <- quarto_manuscript_content_default_statutory_decl
    }

    if ((!title_page) && !(stat_decl)) {
      content_tmp <- quarto_manuscript_content_default
    }

    create_file_with_content(
      file_path = "manuscript.qmd",
      content = content_tmp,
      overwrite = overwrite
    )
  }


  # Presentation files
  if (presentation) {
    if (uma_style) {
      message("\nCreating UMA style presentation qmd...")
      content_tmp <- quarto_presentation_content_uma

      # presentation qmd
      create_file_with_content(
        file_path = "presentation.qmd",
        content = content_tmp,
        overwrite = overwrite
      )

      # theme.scss
      create_file_with_content(
        file_path = "theme.scss",
        content = scss_content,
        overwrite = overwrite
      )
    }

    if (!(uma_style)) {
      message("\nCreating presentation qmd...")
      content_tmp <- quarto_presentation_content_default

      # presentation qmd
      create_file_with_content(
        file_path = "presentation.qmd",
        content = content_tmp,
        overwrite = overwrite
      )
    }
  }


  # .gitignore file
  if (gitignore) {
    message("\nCreating .gitignore file...")

    create_file_with_content(
      file_path = ".gitignore",
      content = gitignore_content,
      overwrite = overwrite
    )
  }


  # code qmd notebookd
  if (code_files) {
    message("\nCreating .qmd code notebooks...")

    create_file_with_content(
      file_path = "code/01_code.qmd",
      content = quarto_code_notebook,
      overwrite = overwrite
    )
  }


  ### --------------------------------------------------------------------------
  ### 7. Finalization
  ### --------------------------------------------------------------------------

  message("\nProject setup complete.")

  # Return an invisible TRUE to indicate success without printing to console
  return(invisible(TRUE))
}
