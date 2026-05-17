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
#' @param department A character string specifying the author's department or school. When `NULL`, no department line is rendered. Defaults to `NULL`.
#' @param mail A character string specifying the author's email address. Included in `manuscript.qmd`. Defaults to `NULL`.
#' @param student_id A character string specifying the student's ID. Included in `manuscript.qmd`. Defaults to `NULL`.
#' @param title A character string specifying the working title of the project. Used in manuscript, presentation, and code files. Defaults to `NULL`.
#' @param subtitle A character string specifying the subtitle of the project. Used in presentation and code files. Defaults to `NULL`.
#' @param title_page Logical. If `TRUE`, generates a native LaTeX title page (`title-page.tex`) included before the manuscript body. Defaults to `FALSE`.
#' @param stat_decl Logical. If `TRUE`, adds a statutory declaration (e.g., for exam papers). Defaults to `FALSE`.
#'
#' @param presentation Logical. If `TRUE`, creates a Quarto Reveal.js presentation (`presentation.qmd`). Defaults to `TRUE`.
#' @param uma_style Logical. If `TRUE`, generates a `theme.scss` and applies it to the presentation. Defaults to `FALSE`.
#' @param theme_color A character string specifying a hex color used as the primary color in `theme.scss`. Only applies when `uma_style = TRUE`. Defaults to `"#333333"`.
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
  department = NULL,
  mail = NULL,
  student_id = NULL,
  title = NULL,
  subtitle = NULL,
  title_page = FALSE,
  stat_decl = FALSE,
  # PRESENTATION SETUP OPTIONS
  presentation = TRUE,
  uma_style = FALSE,
  theme_color = "#333333",
  # Other logistics
  code_files = TRUE,
  data_folders = TRUE,
  gitignore = TRUE,
  overwrite = TRUE
) {
  # --------------------------------------------------------------------------
  # 1. Input Validation and Argument Checks
  # --------------------------------------------------------------------------

  # Ensure a project name is a non-empty character string.
  stopifnot(
    "You must provide a 'project_name'." = project_name != "",
    "The 'project_name' must be a character string." = is.character(
      project_name
    )
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
      "Project directory '",
      full_project_path,
      "' already exists. ",
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

  if (is.null(institution)) {
    institution <- "Your Institution"
  }
  if (is.null(mail)) {
    mail <- "your.email@your.institution.com"
  }
  if (is.null(student_id)) {
    student_id <- "1234567"
  }
  if (is.null(title)) {
    title <- "Untitled Project"
  }
  if (is.null(subtitle)) {
    subtitle <- "A great project"
  }

  # --- Construct the 'author_with_details' string for the YAML header ---
  # The goal is to build a string like: "Firstname Lastname^[Institution; Mail: email; student ID: id]"

  # Check and convert student_id to character if necessary.
  if (!is.null(student_id) && !is.character(student_id)) {
    student_id <- as.character(student_id)
  }

  # Build a vector of details strings only for non-default values.
  details <- c(
    if (!is.null(institution) && institution != "Your Institution") institution,
    if (!is.null(mail) && mail != "your.email@your.institution.com") {
      paste0("Mail: ", mail)
    },
    if (!is.null(student_id) && student_id != "1234567") {
      paste0("Student ID: ", student_id)
    }
  )

  # Combine the details into a single string, separated by semicolons.
  if (length(details) > 0) {
    author_with_details <- paste0(
      author,
      "^[",
      paste(details, collapse = "; "),
      "]"
    )
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
      message(
        "  -> Folder already exists: '",
        folder_path,
        "' (skipping creation)"
      )
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
      message(
        "  -> File already exists (skipping, overwrite = FALSE): '",
        file_path,
        "'"
      )
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
      message(paste(
        "Destination folder does not exist. Creating:",
        dest_folder
      ))
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
      file.copy(
        from = src,
        to = dest_folder,
        recursive = TRUE,
        overwrite = overwrite
      )
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

  # Default PDF Manuscript (No Title Page) -------------------------------------

  quarto_manuscript_content_default <- paste0(
    "---
title: |
  ",
    title,
    "
subtitle: |
  ",
    subtitle,
    "
abstract: |
  You can add an abstract here.
author: \"",
    author_with_details,
    "\"",
    "
thanks: |
   You can add acknowledgements here.
date: last-modified
date-format: MMMM D, YYYY
format:
  pdf:
    keep-tex: false
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
  ",
    title,
    "
subtitle: |
  ",
    subtitle,
    "
abstract: |
  You can add an abstract here.
thanks: |
   You can add acknowledgements here.
date: last-modified
date-format: MMMM D, YYYY
format:
  pdf:
    keep-tex: false
    toc: false
    include-before-body: title-page.tex
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

  # Native LaTeX Title Page Snippet ----------------------------------------------

  title_page_department_line <- if (!is.null(department)) {
    paste0("{", department, "} \\par\n")
  } else {
    ""
  }

  title_page_tex_content <- paste0(
    "\\begin{titlepage}\n",
    "\\centering\n",
    "\\vspace*{2cm}\n",
    "\n",
    "{\\LARGE \\bfseries ", title, " \\par}\n",
    "\\vspace{1cm}\n",
    "{\\Large ", subtitle, " \\par}\n",
    "\\vspace{3cm}\n",
    "\n",
    "{\\large ", author_with_id, " \\par}\n",
    "\\vspace{0.5cm}\n",
    "{", institution, "} \\par\n",
    title_page_department_line,
    "\n",
    "\\vfill\n",
    "\n",
    "{\\today}\n",
    "\\end{titlepage}\n"
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

  department_yaml_line <- if (!is.null(department)) {
    paste0("\n        department: ", department)
  } else {
    ""
  }

  quarto_code_notebook <- paste0(
    "---
title: |
  Code Notebook
subtitle: |
  ",
    title,
    ": ",
    subtitle,
    "
author:
  - name: ",
    author,
    "
    email: ",
    mail,
    "
    affiliations:
      - name: ",
    institution,
    department_yaml_line,
    "
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

  quarto_presentation_content_default <- paste(
    "---
author: ",
    author,
    "
title: ",
    title,
    "
subtitle: ",
    subtitle,
    "
date: last-modified
date-format: MMMM D, YYYY
bibliography: references.bib
biblio-style: apsr
format:
  revealjs:
    embed-resources: true
    slideNumber: true
    footer: ",
    author,
    "  -- {{< meta date >}} -- ",
    title,
    "
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
"
  )

  quarto_presentation_content_uma <- paste(
    "---
author:",
    author,
    "
date: last-modified
date-format: MMMM D, YYYY
bibliography: references.bib
biblio-style: apsr
format:
  revealjs:
    embed-resources: true
    theme: theme.scss
    slideNumber: true
    footer: ",
    author,
    "  -- {{< meta date >}} -- ",
    title,
    "
editor: visual
preview-links: true
---

##",
    title,
    "

###",
    subtitle,
    "

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
"
  )

  scss_content <- paste0(
    "/*-- scss:defaults --*/
$caption-background: ", theme_color, ";
$main-background: white;
$main-text: ", theme_color, ";
$footnote-background: ", theme_color, ";
$presentation-heading-color: ", theme_color, ";

/*-- scss:rules --*/

#title-slide {
  .title {
    color: ", theme_color, ";
  }

  .subtitle {
    color: ", theme_color, ";
  }

  .quarto-title-author {
    color: ", theme_color, ";
  }

  .quarto-title-date {
    color: ", theme_color, ";
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
  color: ", theme_color, ";
}

/* Custom link and list styles */
.reveal a {
  color: ", theme_color, ";
}
.reveal li {
  color: ", theme_color, ";
}
"
  )

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
*code_files*

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

    if (title_page && stat_decl) {
      content_tmp <- quarto_manuscript_content_titlepage_statutory_decl
    } else if (title_page) {
      content_tmp <- quarto_manuscript_content_titlepage
    } else if (stat_decl) {
      content_tmp <- quarto_manuscript_content_default_statutory_decl
    } else {
      content_tmp <- quarto_manuscript_content_default
    }

    create_file_with_content(
      file_path = "manuscript.qmd",
      content = content_tmp,
      overwrite = overwrite
    )

    if (title_page) {
      create_file_with_content(
        file_path = "title-page.tex",
        content = title_page_tex_content,
        overwrite = overwrite
      )
    }
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
