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
  if (grepl("[[:space:]]|[!\"#$%&'()*+,/:;<=>?@[\\]^`{|}~\\-]", project_name)) {
    warning(
      "The 'project_name' contains whitespace or disallowed special characters. ",
      "It is recommended to use a name without these for folder creation (e.g., 'my_project' or 'MyProject')."
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
    image_files_exists <- file.exists(system.file("images", package = "project.setup"))


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
    # Ensure the destination folder exists before attempting to copy items.
    create_folder(dest_folder)

    # Separate existing source items from missing ones for clear feedback.
    existing_sources <- source_paths[file.exists(source_paths)]
    missing_sources <- source_paths[!file.exists(source_paths)]

    # Warn about any source items that could not be found.
    if (length(missing_sources) > 0) {
      warning(
        "Some source items are missing and will not be copied: ",
        paste(sQuote(missing_sources), collapse = ", ")
      )
    }

    if (length(existing_sources) > 0) {
      # Initialize a logical vector to track success for each item.
      success_status <- logical(length(existing_sources))

      # Loop through each source item to handle files and directories appropriately.
      for (i in seq_along(existing_sources)) {
        src <- existing_sources[i] # Current source path

        if (dir.exists(src)) {
          # --- Handle Directories ---
          # When copying a directory, 'to' should be the *parent* directory
          # where the source directory (e.g., '_extensions') will be placed.
          # The result will be 'dest_folder/_extensions'.
          dest_path_for_dir <- file.path(dest_folder, basename(src))

          # If the target directory already exists and overwrite is FALSE, skip.
          if (dir.exists(dest_path_for_dir) && !overwrite) {
            message("  -> Directory already exists (skipping, overwrite = FALSE): '", dest_path_for_dir, "'")
            success_status[i] <- TRUE # Consider it a success if skipped due to no-overwrite
            next # Move to the next item
          } else if (dir.exists(dest_path_for_dir) && overwrite) {
            # If overwriting an existing directory, it's safer to remove it first.
            # This ensures a clean copy and prevents unexpected merging behavior
            # from file.copy's recursive overwrite.
            message("  -> Overwriting existing directory: '", dest_path_for_dir, "' (removing old content)")
            unlink(dest_path_for_dir, recursive = TRUE, force = TRUE)
          }

          # Perform the copy for the directory.
          success_status[i] <- file.copy(
            from = src,
            to = dest_folder, # Copy 'src' (the directory) INTO 'dest_folder'
            recursive = TRUE, # Essential for copying directory contents
            overwrite = overwrite # Applies to files *within* the copied directory
          )
          message("  -> Copied directory: '", src, "' to '", dest_path_for_dir, "'")
        } else if (file.exists(src)) {
          # --- Handle Files ---
          # When copying a file, 'to' should be the full path including the new filename.
          dest_path_for_file <- file.path(dest_folder, basename(src))

          # If the target file already exists and overwrite is FALSE, skip.
          if (file.exists(dest_path_for_file) && !overwrite) {
            message("  -> File already exists (skipping, overwrite = FALSE): '", dest_path_for_file, "'")
            success_status[i] <- TRUE
            next
          }

          # Perform the copy for the file.
          success_status[i] <- file.copy(
            from = src,
            to = dest_path_for_file,
            overwrite = overwrite,
            recursive = FALSE # Not needed for files, but harmless to explicitly state
          )
          message("  -> Copied file: '", src, "' to '", dest_path_for_file, "'")
        } else {
          # This case should ideally not be reached due to the 'existing_sources' filter,
          # but it's here for robustness.
          message("  -> Warning: Source item not found during copy attempt: '", src, "'")
          success_status[i] <- FALSE
        }
      }

      # Provide feedback on the copy operation's overall success.
      if (all(success_status)) {
        message("  -> All specified items copied successfully to '", dest_folder, "'")
      } else {
        # Identify which items failed to copy (e.g., due to permissions).
        failed_copies <- existing_sources[!success_status]
        warning(
          "Some items could not be copied to '", dest_folder, "': ",
          paste(sQuote(failed_copies), collapse = ", "),
          ". This might be due to permissions or other unexpected errors."
        )
      }
    } else {
      message("  -> No existing source items found to copy.")
    }
    invisible(NULL) # Return invisible NULL as this function is for side effects
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

  # Default PDF Manuscript (No Title Page) -------------------------------------

  quarto_manuscript_content_default <- paste0(
    "---
title: |
  ", title, "
subtitle: |
  ", subtitle, "
abstract: |
  You can add an abstract here.
author: \"", author_with_details, "\"", "
thanks: |
   You can add acknowledgements here. Wordcount: {{< words-body >}}.
date: last-modified
date-format: MMMM D, YYYY
format:
  wordcount-pdf:
    toc: false
    include-in-header:
      text: |
        \\usepackage{setspace}
        \\setlength{\\parindent}{15pt}
execute:
  echo: false
  warning: false
  eval: true
  include: true
  cache: true
bibliography: references.bib
biblio-style: apsr
link-citations: true
number-sections: true
papersize: a4
fontsize: 12pt
linestretch: 2
geometry:
  - top = 2cm
  - bottom = 2cm
  - left = 2.5cm
  - right = 2.5cm
  - footskip = 20pt
---

## Introduction {#sec-introduction}

{{< lipsum 2 >}}

## Theory {#sec-theory}

{{< lipsum 2 >}}

## Research Design {#sec-design}

{{< lipsum 2 >}}

## Empirical Analysis {#sec-analysis}

{{< lipsum 2 >}}

## Conclusion {#sec-conclusion}

{{< lipsum 2 >}}

\\singlespacing

## References

::: {#refs}
:::

## Appendix {.appendix}

"
  )


  # If statutory declaration is TRUE

  stat_decl_content <- "
\\newpage

## Eidesstattliche Erklärung -- Statutory Declaration {.unlisted .unnumbered}

\\noindent Hiermit versichere ich, dass diese Arbeit von mir persönlich verfasst ist
und dass ich keinerlei fremde Hilfe in Anspruch genommen habe. Ebenso
versichere ich, dass diese Arbeit oder Teile daraus weder von mir selbst
noch von anderen als Leistungsnachweise andernorts eingereicht wurden.
Wörtliche oder sinngemäße Übernahmen aus anderen Schriften und
Veröffentlichungen in gedruckter oder elektronischer Form sind
gekennzeichnet. Sämtliche Sekundärliteratur und sonstige Quellen sind
nachgewiesen und in der Bibliographie aufgeführt. Das Gleiche gilt für
graphische Darstellungen und Bilder sowie für alle Internet-Quellen. Ich
bin ferner damit einverstanden, dass meine Arbeit zum Zwecke eines
Plagiatsabgleichs in elektronischer Form anonymisiert versendet und
gespeichert werden kann. Mir ist bekannt, dass von der Korrektur der
Arbeit abgesehen und die Prüfungsleistung mit „nicht ausreichend“
bewertet werden kann, wenn die Erklärung nicht erteilt wird.


\\noindent I hereby declare that the paper presented is my own work and that I have
not called upon the help of a third party. In addition, I affirm that neither I
nor anybody else has submitted this paper or parts of it to obtain credits
elsewhere before. I have clearly marked and acknowledged all quotations
or references that have been taken from the works of other. All secondary
literature and other sources are marked and listed in the bibliography. The
same applies to all charts, diagrams and illustrations as well as to all Internet
sources. Moreover, I consent to my paper being electronically stores and
sent anonymously in order to be checked for plagiarism. I am aware that
the paper cannot be evaluated and may be graded “failed” (“nicht
                                                           ausreichend”) if the declaration is not made.

```{=latex}
\\vspace{2cm}
\\noindent
\\parbox{5cm}{
  \\hrulefill\\\\
  Place, Date
}
\\hfill
\\parbox{5cm}{
  \\hrulefill\\\\
  Signature
}
```
"


  quarto_manuscript_content_default_statutory_decl <- paste0(
    quarto_manuscript_content_default,
    stat_decl_content
  )


  # PDF Manuscript with Title Page -----------------------------------------------

  quarto_manuscript_content_titlepage <- paste0(
    "---
title: |
  ", title, "
subtitle: |
  ", subtitle, "
abstract: |
  You can add an abstract here.
author:
  - name: ", author_with_id, "
    email: ", mail, "
    affiliations:
      - name: ", institution, "
        department: School of Social Sciences
thanks: |
   You can add acknowledgements here. Wordcount: {{< words-body >}}.
date: last-modified
date-format: MMMM D, YYYY
format:
  titlepage-pdf:
    citeproc: false
    filters:
      - at: pre-quarto
        path: _extensions/andrewheiss/wordcount/citeproc.lua
      - at: pre-quarto
        path: _extensions/andrewheiss/wordcount/wordcount.lua
    titlepage: academic
    toc: false
    include-in-header:
      text: |
        \\usepackage{setspace}
        \\setlength{\\parindent}{15pt}
execute:
  echo: false
  warning: false
  eval: true
  include: true
  cache: true
bibliography: references.bib
biblio-style: apsr
link-citations: true
number-sections: true
papersize: a4
fontsize: 12pt
linestretch: 2
geometry:
  - top = 2cm
  - bottom = 2cm
  - left = 2.5cm
  - right = 2.5cm
  - footskip = 20pt
---

## Introduction {#sec-introduction}

{{< lipsum 2 >}}

## Theory {#sec-theory}

{{< lipsum 2 >}}

## Research Design {#sec-design}

{{< lipsum 2 >}}

## Empirical Analysis {#sec-analysis}

{{< lipsum 2 >}}

## Conclusion {#sec-conclusion}

{{< lipsum 2 >}}

\\singlespacing

## References

::: {#refs}
:::

## Appendix {.appendix}

"
  )




  quarto_manuscript_content_titlepage_statutory_decl <- paste0(
    quarto_manuscript_content_titlepage,
    stat_decl_content
  )



  # PDF Manuscript with Title Page and Logo --------------------------------------

  quarto_manuscript_content_titlepage_logo <- paste0(
    "---
title: |
  ", title, "
subtitle: |
  ", subtitle, "
abstract: |
  You can add an abstract here.
author:
  - name: ", author_with_id, "
    email: ", mail, "
    affiliations:
      - name: ", institution, "
        department: School of Social Sciences
thanks: |
   You can add acknowledgements here. Wordcount: {{< words-body >}}.
date: last-modified
date-format: MMMM D, YYYY
format:
  titlepage-pdf:
    citeproc: false
    filters:
      - at: pre-quarto
        path: _extensions/andrewheiss/wordcount/citeproc.lua
      - at: pre-quarto
        path: _extensions/andrewheiss/wordcount/wordcount.lua
    titlepage: academic
    titlepage-logo: ", logo_path, "
    toc: false
    include-in-header:
      text: |
        \\usepackage{setspace}
        \\setlength{\\parindent}{15pt}
execute:
  echo: false
  warning: false
  eval: true
  include: true
  cache: true
bibliography: references.bib
biblio-style: apsr
link-citations: true
number-sections: true
papersize: a4
fontsize: 12pt
linestretch: 2
geometry:
  - top = 2cm
  - bottom = 2cm
  - left = 2.5cm
  - right = 2.5cm
  - footskip = 20pt
---

## Introduction {#sec-introduction}

{{< lipsum 2 >}}

## Theory {#sec-theory}

{{< lipsum 2 >}}

## Research Design {#sec-design}

{{< lipsum 2 >}}

## Empirical Analysis {#sec-analysis}

{{< lipsum 2 >}}

## Conclusion {#sec-conclusion}

{{< lipsum 2 >}}

\\singlespacing

## References

::: {#refs}
:::

## Appendix {.appendix}

"
  )


  quarto_manuscript_content_titlepage_logo_statutory_decl <- paste0(
    quarto_manuscript_content_titlepage_logo,
    stat_decl_content
  )


  # References Bibtex File -------------------------------------------------------

  ref_bib <- paste0(
    "@article{article_key_here, \n",
    "  author = {Lastname, Firstname and Lastname, Firstname}, \n",
    "  title = {Title of the Article}, \n",
    "  journal = {Journal Title}, \n",
    "  year = {YYYY}, \n",
    "  volume = {1}, \n",
    "  number = {1}, \n",
    "  pages = {1-10}, \n",
    "  doi = {doi:10.1234/56789}\n",
    "}\n\n",
    "@book{book_key_here, \n",
    "  author = {Lastname, Firstname}, \n",
    "  title = {Title of the Book}, \n",
    "  publisher = {Publisher Name}, \n",
    "  address = {City, Country}, \n",
    "  year = {YYYY}, \n",
    "}\n\n",
    "@incollection{incollection_key_here, \n",
    "  author = {Lastname, Firstname}, \n",
    "  title = {Title of the Chapter}, \n",
    "  booktitle = {Title of the Edited Book}, \n",
    "  editor = {Lastname, Firstname}, \n",
    "  publisher = {Publisher Name}, \n",
    "  address = {City, Country}, \n",
    "  year = {YYYY}, \n",
    "  pages = {1-10}, \n",
    "}\n"
  )

  # Code QMD ---------------------------------------------------------------------

  quarto_code_notebook <- paste0(
    "---
title: |
  Code Notebook
subtitle: |
  ", title, ": ", subtitle, "
author:
  - name: ", author, "
    email: ", mail, "
    affiliations:
      - name: ", institution, "
        department: School of Social Sciences
date: last-modified
date-format: MMMM D, YYYY
format:
  html:
    toc: true
    code-fold: true
    code-tools: true
execute:
  echo: true
  warning: true
  eval: true
  message: true
---

# Setup

```{r}
#| label: setup

# To track render duration
start_time <- Sys.time()

# set width of console output
options(width = 80)


# Install and load required packages
p_required <- c(
  \"tidyverse\",
  \"here\",
  \"sessioninfo\"
)
packages <- rownames(installed.packages())
p_to_install <- p_required[!(p_required %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_required, require, character.only = TRUE)
rm(p_required, p_to_install, packages)
```

# Code 1

```{r}
#| label: code-1

# start your code here

```


# Session Info

```{r}
#| label: session-info

session_info()
```


# Render Time

```{r}
#| label: render-time

end_time <- Sys.time()

rendering_time <- end_time - start_time

message(paste(\"Document rendered in:\", round(as.numeric(rendering_time, units = \"secs\"), 2), \"seconds.\n\"))
```


  "
  )



  # Presentation QMD -------------------------------------------------------------

  quarto_presentation_content_default <- paste("---
author: ", author, "
title: ", title, "
subtitle: ", subtitle, "
date: last-modified
date-format: MMMM D, YYYY
bibliography: references.bib
biblio-style: apsr
format:
  revealjs:
    embed-resources: true
    slideNumber: true
    footer: ", author, "  -- {{< meta date >}} -- ", title, "
preview-links: true
---



# Introduction

## Motivation

- Bullet point 1
- more details @ [Quarto Revealjs Documentation](https://quarto.org/docs/presentations/revealjs/)

------------------------------------------------------------------------

## Relevance

------------------------------------------------------------------------

## Research Question

# Theory

## Prior Research

------------------------------------------------------------------------

## Theoretical Framework

------------------------------------------------------------------------

## Argument

# Research Design

------------------------------------------------------------------------

## Data

------------------------------------------------------------------------

## Methods

# Results

------------------------------------------------------------------------

## Results I

------------------------------------------------------------------------

## Results II

# Conclusion

------------------------------------------------------------------------

## Summary

------------------------------------------------------------------------

## Implications

# Thank you for your attention!

------------------------------------------------------------------------

## References
")


  quarto_presentation_content_uma <- paste("---
author:", author, "
date: last-modified
date-format: MMMM D, YYYY
bibliography: references.bib
biblio-style: apsr
format:
  revealjs:
    embed-resources: true
    theme: theme.scss
    slideNumber: true
    footer: Tristan Muno <U+2013> {{< meta date >}} <U+2013> Course
    logo: ", logo_path, "
editor: visual
preview-links: true
---

##", title, "

###", subtitle, "

![](", title_image_path, "){width=\"100%\"}
{{< meta author >}}<br>
{{< meta date >}}

# Introduction

## Motivation

- Bullet point 1
- more details @ [Quarto Revealjs Documentation](https://quarto.org/docs/presentations/revealjs/)

------------------------------------------------------------------------

## Relevance

------------------------------------------------------------------------

## Research Question

# Theory

## Prior Research

------------------------------------------------------------------------

## Theoretical Framework

------------------------------------------------------------------------

## Argument

# Research Design

------------------------------------------------------------------------

## Data

------------------------------------------------------------------------

## Methods

# Results

------------------------------------------------------------------------

## Results I

------------------------------------------------------------------------

## Results II

# Conclusion

------------------------------------------------------------------------

## Summary

------------------------------------------------------------------------

## Implications

# Thank you for your attention!

------------------------------------------------------------------------

## References
")


  scss_content <- "/*-- scss:defaults --*/
$caption-background: #003056;
$main-background: white;
$main-text: #003056;
$footnote-background: #003056;
$presentation-heading-color: #003056;

/*-- scss:rules --*/
/*.reveal .slides > section > h1, .reveal .slides > section > section > h2 {
    background-color: $caption-background;
    color: $main-background;
} */

#title-slide {
  .title {
    color: #003056; /* This is the fill color for the inside of the text */
  /*  -webkit-text-stroke: 1px #DE7E50; /* This adds the border color */
  /*  text-stroke: 1px #DE7E50; /* For non-WebKit browsers */
  }

  .subtitle {
    color: #003056;
  /*  -webkit-text-stroke: 1px #DE7E50;
  /*  text-stroke: 1px #DE7E50; */
  }

  .quarto-title-author {
    color: #003056;
  /*  -webkit-text-stroke: 1px #DE7E50;
    text-stroke: 1px #DE7E50; */
  }

  .quarto-title-date {
    color: #003056;
   /* -webkit-text-stroke: 1px #DE7E50;
    text-stroke: 1px #DE7E50; */
  }
}

.reveal .slides > section > p, .reveal .slides > section > section > p {
    color: $main-text;
}

.reveal .slide-number {
    background-color: $footnote-background;
    color: $main-text;
    bottom: 14px !important;
    right: 50px !important;
    top: unset !important;
}

.reveal .footer {
    background-color: $main-background;
    color: $main-text;
}

/* Custom color for author and date */
.quarto-author, .quarto-date {
  color: #003056; /* Change to your desired color */
}

/* Custom link and list styles */
.reveal a {
  color: #DE7E50;
}
.reveal li {
  color: #003056;
}

/* Adjust the logo size */
.reveal .slide-logo {
        max-height: 4em !important;
        top: 0;
        right: 12px
      }
"


  # Gitignore file ---------------------------------------------------------------

  gitignore_content <- "
# IDE and R-specific files
.Rproj.user
.Rhistory
.RData
.Ruserdata

# Quarto
.quarto
_extensions/
_freeze/
_publish.yml
**/.ipynb_checkpoints/

# Cache folders and files
*cache*

# Miscellaneous
.DS_Store
Thumbs.db
"


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
      source_paths = system.file("_extensions", package = "project.setup"),
      dest_folder = getwd(),
      overwrite = overwrite
    )

    copy_items(
      source_paths = system.file("images", package = "project.setup"),
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
