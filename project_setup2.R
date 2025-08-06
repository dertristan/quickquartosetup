#' -----------------------------------------------------------------------------
#' PROJECT SETUP FUNCTION ------------------------------------------------------
#' -----------------------------------------------------------------------------





project_setup <- function(
    # Name to give to the overall project
  project_name = "",
  path = ".",
  # MANUSCRIPT SETUP OPTIONS
  manuscript = TRUE, # possibility of disabling manuscript qmd creation
  author = NULL, # Name of author
  institution = NULL, # add institution if wanted
  mail = NULL, # add mail
  student_id = NULL, # add student id
  title = NULL, # Working title
  subtitle = NULL, # Working Subtitle
  title_page = FALSE, # have dedicated title page
  logo = FALSE, # have logo displayed on manuscript
  stat_decl = FALSE, # add statutory declaration for examination term paper
  # PRESENTATION SETUP OPTIONS
  presentation = TRUE, # create presentation qmd
  uma_style = TRUE, # uni MA style for presentation qmd
  title_image_path = "C:/R/logistics/project_setup/images/uma_palace.png",
  logo_path = "C:/R/logistics/project_setup/images/uma_ss.png",
  # Other logistics
  code_files = TRUE, # create code documentation qmd files
  data_folders = TRUE, # create data folders
  gitignore = TRUE, # create gitignore file
  overwrite = TRUE # overwrite existing folders and files
) {
  
  
  
  ### --------------------------------------------------------------------------
  ### 1. Input Validation and Argument Checks
  ### --------------------------------------------------------------------------
  
  # Ensure a project name is provided
  if (project_name == "") {
    stop("You must provide a 'project_name'.")
  }
  
  # Ensure project name is character
  if (!is.character(project_name)) {
    stop("No character value provided to 'project_name'.")
  }
  
  # Construct the full project path
  full_project_path <- file.path(path, project_name)
  
  # Update the validation check to use the full path
  if (dir.exists(full_project_path) && !overwrite) {
    stop(
      "Project directory '", full_project_path, "' already exists. ",
      "Set `overwrite = TRUE` to continue (files may be overwritten)."
    )
  }
  
  # Handle NULL values for metadata by providing sensible defaults
  if (is.null(author)) {
    # A robust, cross-platform way to get the username
    author <- Sys.getenv("LOGNAME")
    if (author == "") {
      author <- Sys.getenv("USER")
    }
    if (author == "") {
      author <- Sys.getenv("USERNAME")
    }
    # Final fallback if none of the above are set
    if (author == "") {
      author <- "Your Name Here"
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
  # The goal is to build a string like:
  # "Firstname Lastname^[Institution; Mail: email; student ID: id]"
  
  # First, create a vector of the optional details
  details_list <- c()
  
  # Check and convert student_id to character if necessary
  if (!is.null(student_id) && !is.character(student_id)) {
    student_id <- as.character(student_id)
  }
  
  # Add each detail to the list only if it's not a placeholder
  if (!is.null(institution) && institution != "Your Institution") {
    details_list <- c(details_list, institution)
  }
  if (!is.null(mail) && mail != "your.email@your.institution.com") {
    details_list <- c(details_list, paste0("Mail: ", mail))
  }
  if (!is.null(student_id) && student_id != "1234567") {
    details_list <- c(details_list, paste0("Student ID: ", student_id))
  }
  
  # Combine the details into a single string, separated by semicolons
  if (length(details_list) > 0) {
    details_string <- paste(details_list, collapse = "; ")
    author_with_details <- paste0(author, "^[", details_string, "]")
  } else {
    # If no details were provided, just use the author's name
    author_with_details <- author
  }
  
  
  # --- Construct the 'author_with_id' variable ---
  # The goal is to build a string like:
  # "Firstname Lastname (Student ID)"
  if (!is.null(student_id) && student_id != "1234567") {
    author_with_id <- paste0(author, " (", student_id, ")")
  } else {
    # If no student ID was provided, just use the author's name
    author_with_id <- author
  }
  
  
  # Check for the existence of template images if presentation is TRUE
  # and the UMA style is requested.
  if (presentation && uma_style) {
    
    
    # Check if each file exists
    logo_file_check <- !file.exists(logo_path)
    image_file_check <- !file.exists(title_image_path)
    
    if (logo_file_check || image_file_check) {
      # If any images are missing, throw a warning and disable the UMA style
      warning(
        " Logo or Title Image File not Found:",
        ".\nPresentation will still be created, but without the UMA style templates."
      )
      # Set uma_style to FALSE to prevent the function from trying to copy them later
      uma_style <- FALSE
    }
  }
  ### --------------------------------------------------------------------------
  ### 3. Helper Functions (Encapsulated)
  ### --------------------------------------------------------------------------

  # Function to create folders if they don't exist
  # dir.create() with recursive = TRUE
  # handles existing folders without issue.
  create_folder <- function(folder_path) {
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
      message("  -> Created folder: '", folder_path, "'")
    } else {
      message("  -> Folder already exists: '", folder_path, "'")
    }
  }

  # Function to create files with specified content, respecting the `overwrite` flag.
  create_file_with_content <- function(file_path, content, overwrite) {
    # Check if the file exists and whether we are allowed to overwrite
    if (!file.exists(file_path) || overwrite) {
      writeLines(content, file_path)
      if (file.exists(file_path) && overwrite) {
        message("  -> Overwrote existing file: '", file_path, "'")
      } else {
        message("  -> Created file: '", file_path, "'")
      }
    } else {
      message("  -> File already exists (skipping): '", file_path, "'")
    }
  }

  # Function to copy images, respecting the `overwrite` flag.
  copy_files <- function(source_paths, dest_folder, overwrite) {
    create_folder(dest_folder) # Ensure destination exists

    # Filter out missing source files before attempting to copy
    existing_sources <- source_paths[file.exists(source_paths)]
    missing_sources <- source_paths[!file.exists(source_paths)]

    # Provide feedback on missing files
    if (length(missing_sources) > 0) {
      warning(
        "Some source files are missing and will not be copied: ",
        paste(missing_sources, collapse = ", ")
      )
    }

    if (length(existing_sources) > 0) {
      # Attempt to copy the images that do exist
      success <- sapply(names(existing_sources), function(name) {
        file.copy(
          from = existing_sources[[name]],
          to = file.path(dest_folder, basename(existing_sources[[name]])),
          overwrite = overwrite
        )
      })

      if (all(success)) {
        message("  -> All files copied successfully!")
      } else {
        # This case would indicate a permissions issue, not a missing file issue
        warning(" -> Some files could not be copied due to an unexpected error.")
      }
    } else {
      message("  -> No files found to copy.")
    }
  }


  ### --------------------------------------------------------------------------
  ### 4. Project Directory Creation and Scoping
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
  "@article{, \n",
  "  author = {},\n",
  "  title = {},\n",
  "  journal = {},\n",
  "  year = {},\n",
  "  volume = {},\n",
  "  number = {},\n",
  "  pages = {},\n",
  "  doi = {}\n",
  "}"
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


  ")



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
    footer: ", author, "  – {{< meta date >}} – ", title, "
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
    footer: Tristan Muno – {{< meta date >}} – Course
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
    core_folders <- c("data", "code")
    lapply(core_folders, create_folder)
  }

  # --- Conditional File Creation ---

  # Manuscript files
  if (manuscript) {
    message("\nCreating manuscript files...")
    create_file_with_content("manuscript/manuscript.qmd", quarto_manuscript_content, overwrite)
  }

  # Presentation files
  if (presentation) {
    message("\nCreating presentation files...")
    # Create presentation subfolders first
    create_folder("presentation/images")

    create_file_with_content("presentation/presentation.qmd", quarto_presentation_content, overwrite)

    # Conditionally create UMA style theme and copy images
    if (uma_style) {
      message("\nApplying UMA style...")
      create_file_with_content("presentation/theme.scss", scss_content, overwrite)

      image_source_paths <- list(
        title_image = title_image_path,
        logo = logo_path
      )
      copy_images(image_source_paths, "presentation/images", overwrite)
    }
  }

  # Code documentation files
  if (code_files) {
    message("\nCreating code documentation files...")
    # ... your calls to create_file_with_content() for the code files go here ...
  }

  # Bibliography files
  if (references) {
    message("\nCreating bibliography files...")
    # ... your call to create_file_with_content() for the .bib files goes here ...
  }

  # .gitignore file
  if (gitignore) {
    message("\nCreating .gitignore file...")
    gitignore_content <- c(...) # your gitignore string
    create_file_with_content(".gitignore", gitignore_content, overwrite)
  }


  ### --------------------------------------------------------------------------
  ### 7. Finalization
  ### --------------------------------------------------------------------------

  message("\nProject setup complete.")

  # Return an invisible TRUE to indicate success without printing to console
  return(invisible(TRUE))
}
