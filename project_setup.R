# PROJECT SETUP SCRIPT #
########################

# Title and Author Variables
title <- "This Could Become Your Next Title"
subtitle <- "This Could Become Your Next Subtitle"
author <- "Tristan Muno"

# Define paths for easier customization
image_source_paths <- list(
  schloss = "C:/R/templates/schloss_tmpl.PNG",
  logo = "C:/R/templates/uma_ss.png"
)
folders <- c("code", "data", "manuscript", "presentation")
presentation_folder <- "presentation/images"
biblio_file_paths <- c("manuscript/literature.bib", "presentation/literature.bib")

# Function to create folders
create_folder <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message("Created folder: ", folder_path)
  } else {
    message("Folder already exists: ", folder_path)
  }
}

# Function to create files if they do not exist
create_file_if_not_exists <- function(file_path, content) {
  if (!file.exists(file_path)) {
    writeLines(content, file_path)
    message("File created: ", file_path)
  } else {
    message("File already exists: ", file_path)
  }
}

# Function to copy images to the presentation folder
copy_images <- function(source_paths, dest_folder) {
  create_folder(dest_folder) # Ensure destination exists
  success <- sapply(names(source_paths), function(name) {
    file.copy(source_paths[[name]], file.path(dest_folder, basename(source_paths[[name]])), overwrite = TRUE)
  })
  if (all(success)) {
    message("Images copied successfully!")
  } else {
    warning("Some images could not be copied.")
  }
}

# Create main folders and images subfolder
lapply(folders, create_folder)
create_folder(presentation_folder)

# Git ignore content ####
gitignore_content <- c(
  ".Rproj.user",  # Ignore RStudio project files
  ".Rhistory",    # Ignore R history files
  ".RData",       # Ignore R data files
  ".Ruserdata"    # Ignore RStudio user data
)

# Create .gitignore file
create_file_if_not_exists(".gitignore", gitignore_content)

# Quarto manuscript template content ####
quarto_manuscript_content <- paste("---
title:", title, "
subtitle:", subtitle, " 
author:", author, " 
thanks: I thank the world.
date: last-modified
date-format: MMMM D, YYYY
abstract: This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract. This could become your next abstract.

format: 
  pdf:
    toc: false
    keep-tex: true
biblio-style: apsr
bibliography: literature.bib
link-citations: true
colorlinks: true
linkcolor: blue
papersize: a4
fontsize: 12pt
geometry:
  - top=2cm
  - bottom=2cm
  - left=2.5cm
  - right=2.5cm
  - footskip=20pt
header-includes:
  - \\usepackage{setspace}
  - \\setlength{\\parindent}{15pt}
---

\\doublespacing

# Intro

This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction.

This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction. This is the introduction.

# Theory

This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. @hahm2024divided.

This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section. This is the theory section.

# Research Design

This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section.

This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section. This is the research design section.

# Results

This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section.

This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section. This is the results section.

# Conclusion

This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion. This is the conclusion.

# References {.unlisted .unnumbered}

::: {#refs}
:::

\\newpage

# Appendix {.appendix}

This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix. This is the appendix.
")

# Quarto presentation template content ####
quarto_presentation_content <- paste("--- 
author:", author, " 
date: last-modified
date-format: MMMM D, YYYY
bibliography: literature.bib
biblio-style: apsr
format:
  revealjs:
    embed-resources: true
    theme: theme.scss
    slideNumber: true
    footer: Tristan Muno – {{< meta date >}} – Course
    logo: images/uma_ss.png
editor: visual
preview-links: true
---

##", title, "

![](images/schloss_tmpl.PNG){width=\"100%\"} 
{{< meta author >}}
{{< meta date >}}

# Introduction

## Motivation

- Bullet point 1

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

# SCSS theme content ####
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

# Bibliography template content ####
biblio_content <- "
@article{hahm2024divided,
  title={Divided we unite: The nature of partyism and the role of coalition partnership in Europe},
  author={Hahm, Hyeonho and Hilpert, David and K{\"o}nig, Thomas},
  journal={American Political Science Review},
  volume={118},
  number={1},
  pages={69--87},
  year={2024},
  publisher={Cambridge University Press}
}
"

# Quarto code content ####
quarto_code1_content <-  paste("---
title:", title, "
subtitle: Code Documentation 1 – Data Collection 
author:", author, " 
thanks: I thank the world.
date: last-modified
date-format: MMMM D, YYYY
code-line-numbers: true
execute:
  echo: true
  warning: true
format: 
  pdf:
    toc: false
    keep-tex: true
link-citations: true
colorlinks: true
linkcolor: blue
papersize: a4
fontsize: 12pt
geometry:
  - top=2cm
  - bottom=2cm
  - left=2.5cm
  - right=2.5cm
  - footskip=20pt
header-includes:
  - \\usepackage{setspace}
  - \\setlength{\\parindent}{15pt}
---

# Setup

```{r}
#| label: setup

##############################
## TIME TO RENDER: tba #######
##############################

# set width of console output
options(width = 90)


# Install and load required packages
p_required <- c(
  \"tidyverse\", # for dplyr, ggplot & co
  \"here\", # to not worry about working directories
  \"benchmarkme\" # for system info
)
packages <- rownames(installed.packages())
p_to_install <- p_required[!(p_required %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_required, require, character.only = TRUE)
rm(p_required, p_to_install, packages)
```

# Data Collection

# System Information

```{r}
#| label: system-info

get_sys_details()
```
")

quarto_code2_content <-  paste("---
title:", title, "
subtitle: Code Documentation 2 – Data Wrangling
author:", author, " 
thanks: I thank the world.
date: last-modified
date-format: MMMM D, YYYY
code-line-numbers: true
execute:
  echo: true
  warning: true
format: 
  pdf:
    toc: false
    keep-tex: true
link-citations: true
colorlinks: true
linkcolor: blue
papersize: a4
fontsize: 12pt
geometry:
  - top=2cm
  - bottom=2cm
  - left=2.5cm
  - right=2.5cm
  - footskip=20pt
header-includes:
  - \\usepackage{setspace}
  - \\setlength{\\parindent}{15pt}
---

# Setup

```{r}
#| label: setup

##############################
## TIME TO RENDER: tba #######
##############################

# set width of console output
options(width = 90)


# Install and load required packages
p_required <- c(
  \"tidyverse\", # for dplyr, ggplot & co
  \"here\", # to not worry about working directories
  \"benchmarkme\" # for system info
)
packages <- rownames(installed.packages())
p_to_install <- p_required[!(p_required %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_required, require, character.only = TRUE)
rm(p_required, p_to_install, packages)
```

# Data Wrangling

# System Information

```{r}
#| label: system-info

get_sys_details()
```
")

quarto_code3_content <-  paste("---
title:", title, "
subtitle: Code Documentation 3 – Data Analysis 
author:", author, " 
thanks: I thank the world.
date: last-modified
date-format: MMMM D, YYYY
code-line-numbers: true
execute:
  echo: true
  warning: true
format: 
  pdf:
    toc: false
    keep-tex: true
link-citations: true
colorlinks: true
linkcolor: blue
papersize: a4
fontsize: 12pt
geometry:
  - top=2cm
  - bottom=2cm
  - left=2.5cm
  - right=2.5cm
  - footskip=20pt
header-includes:
  - \\usepackage{setspace}
  - \\setlength{\\parindent}{15pt}
---

# Setup

```{r}
#| label: setup

##############################
## TIME TO RENDER: tba #######
##############################

# set width of console output
options(width = 90)


# Install and load required packages
p_required <- c(
  \"tidyverse\", # for dplyr, ggplot & co
  \"here\", # to not worry about working directories
  \"benchmarkme\" # for system info
)
packages <- rownames(installed.packages())
p_to_install <- p_required[!(p_required %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_required, require, character.only = TRUE)
rm(p_required, p_to_install, packages)
```

# Data Wrangling

# System Information

```{r}
#| label: system-info

get_sys_details()
```
")

# Create files for manuscript, presentation, and theme
create_file_if_not_exists("manuscript/manuscript.qmd", quarto_manuscript_content)
create_file_if_not_exists("presentation/presentation.qmd", quarto_presentation_content)
create_file_if_not_exists("presentation/theme.scss", scss_content)
create_file_if_not_exists("code/01_data_collection.qmd", quarto_code1_content)
create_file_if_not_exists("code/02_data_wrangling.qmd", quarto_code2_content)
create_file_if_not_exists("code/03_data_analysis.qmd", quarto_code3_content)



# Create bibliography files
lapply(biblio_file_paths, function(file_path) {
  create_file_if_not_exists(file_path, biblio_content)
})

# Copy images
copy_images(image_source_paths, presentation_folder)

# Clean Environment
rm(folders, gitignore_content, quarto_manuscript_content, quarto_presentation_content, scss_content, 
   biblio_content, biblio_file_paths, image_source_paths, presentation_folder,
   create_folder, create_file_if_not_exists, copy_images,
   quarto_code1_content, quarto_code2_content, quarto_code3_content,
   author, title, subtitle)

cat("\nSetup Complete\n")
