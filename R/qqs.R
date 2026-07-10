#' Quick Quarto Setup
#'
#' Scaffolds a standardized research project folder structure (manuscript, presentation,
#' code notebook, data folders, bibliography, `_quarto.yml`) directly into the current
#' working directory. Pre-existing files in the working directory are left untouched;
#' the `overwrite` flag only governs files that `qqs()` itself generates.
#'
#' @param manuscript Logical. If `TRUE`, creates a Quarto manuscript file (`manuscript.qmd`). Defaults to `TRUE`.
#' @param tutorial Logical. If `TRUE`, populates `manuscript.qmd` with live, rendering Quarto examples (prose formatting, a table, a figure, equations, cross-references, and citations) instead of the plain placeholder body. Defaults to `FALSE`.
#' @param author A character string specifying the author's name. Used in the project-level `_quarto.yml`. Defaults to `NULL` (system username).
#' @param mail A character string specifying the author's email address. When non-default, included as a footnote on the author. Defaults to `NULL`.
#' @param student_id A character string specifying the student's ID. When non-default, included as a footnote on the author. Defaults to `NULL`.
#' @param title A character string specifying the working title of the project. Used in manuscript and presentation. Defaults to `NULL`.
#' @param subtitle A character string specifying the subtitle of the project. Used in manuscript and presentation. Defaults to `NULL`.
#' @param titlepage Logical. If `TRUE`, generates a native LaTeX title page (`title-page.tex`) and arranges the manuscript front matter as cover page, then abstract, then table of contents, before the body. The title page is a single, easy-to-edit `.tex` file. Defaults to `FALSE`.
#' @param stat_decl Logical. If `TRUE`, adds a bilingual (German/English) statutory declaration (e.g., for exam papers). Defaults to `FALSE`.
#'
#' @param presentation Logical. If `TRUE`, creates a minimal Quarto Reveal.js presentation (`presentation.qmd`). Defaults to `TRUE`.
#'
#' @param code_files Logical. If `TRUE`, creates a code template file (`code/00_code_template.qmd`) that students can copy and rename for each analysis. Defaults to `TRUE`.
#' @param data_folders Logical. If `TRUE`, creates standard data subfolders (`01_raw`, `02_processed`, `03_final`). Defaults to `TRUE`.
#' @param gitignore Logical. If `TRUE`, generates a `.gitignore` file. Defaults to `TRUE`.
#' @param overwrite Logical. If `TRUE`, allows overwriting files that `qqs()` itself generates. Pre-existing user files are never touched. Defaults to `TRUE`.
#'
#' @export

qqs <- function(
  # MANUSCRIPT SETUP OPTIONS
  manuscript = TRUE,
  tutorial = FALSE,
  author = NULL,
  mail = NULL,
  student_id = NULL,
  title = NULL,
  subtitle = NULL,
  titlepage = FALSE,
  stat_decl = FALSE,
  # PRESENTATION SETUP OPTIONS
  presentation = TRUE,
  # Other logistics
  code_files = TRUE,
  data_folders = TRUE,
  gitignore = TRUE,
  overwrite = TRUE
) {
  # --------------------------------------------------------------------------
  # 1. Argument Defaults
  # --------------------------------------------------------------------------

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
  # The goal is to build a string like: "Firstname Lastname^[Mail: email; Student ID: id]"

  # Check and convert student_id to character if necessary.
  if (!is.null(student_id) && !is.character(student_id)) {
    student_id <- as.character(student_id)
  }

  # Build a vector of details strings only for non-default values.
  details <- c(
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

  ### --------------------------------------------------------------------------
  ### 3. Project Setup Initialization
  ### --------------------------------------------------------------------------

  message("\nStarting project setup in: '", getwd(), "'.")

  ### --------------------------------------------------------------------------
  ### 5. Define All File Content Strings
  ### --------------------------------------------------------------------------

  # The manuscript is composed from independent parts (YAML front matter + body +
  # shared trailer + optional statutory declaration) so that the `titlepage`,
  # `tutorial`, and `stat_decl` flags combine freely without a combinatorial
  # explosion of precomputed template strings.

  # Manuscript YAML: plain PDF (default, no title page) ---------------------------

  manuscript_yaml_default <- paste0(
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
format:
  pdf:
    keep-tex: false
    toc: false
    include-in-header:
      text: |
        \\usepackage{setspace}
        \\setlength{\\parindent}{15pt}
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

"
  )

  # Manuscript YAML: title-page variant ------------------------------------------
  # Adds a cover page (title-page.tex) before the body and turns the table of
  # contents on, giving the flow: cover -> abstract (via \\maketitle) -> TOC ->
  # body. The date set in _quarto.yml is rendered by \\maketitle on the abstract
  # page; the cover uses LaTeX's built-in \\today.

  manuscript_yaml_titlepage <- paste0(
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
format:
  pdf:
    keep-tex: false
    toc: true
    include-before-body: title-page.tex
    include-in-header:
      text: |
        \\usepackage{setspace}
        \\setlength{\\parindent}{15pt}
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

"
  )

  # Shared authoring-help note shown at the top of the manuscript body -----------

  manuscript_intro <- "> See [Quarto authoring with Positron](https://quarto.org/docs/get-started/authoring/positron.html) for help with manuscript authoring.\n\n"

  # Manuscript body: plain placeholder (default) ---------------------------------

  manuscript_body_lipsum <- "## Introduction {#sec-introduction}

{{< lipsum 2 >}}

## Theory {#sec-theory}

{{< lipsum 2 >}}

## Research Design {#sec-design}

{{< lipsum 2 >}}

## Empirical Analysis {#sec-analysis}

{{< lipsum 2 >}}

## Conclusion {#sec-conclusion}

{{< lipsum 2 >}}
"

  # Manuscript body: live Quarto tutorial ----------------------------------------
  # Single-quoted so the many double quotes in code chunks need no escaping.
  # Backslashes are still doubled because R interprets escapes in both quote
  # styles. Everything here renders with base R + knitr only (no new packages).

  manuscript_body_tutorial <- '## Introduction {#sec-introduction}

This manuscript is a live tutorial: every feature below is written in
[Quarto](https://quarto.org) Markdown and renders straight to PDF. Read this
`.qmd` source next to the rendered output to see exactly how each element is
written.

### Prose formatting

Text can be **bold**, *italic*, or `monospaced`, and lists are simple:

- A first point
- A second point that links to the [Quarto website](https://quarto.org)

> Use a block quote to highlight an important statement.

### Citations

Cite a work in parentheses [@article_key_here] or directly in the text as
@book_key_here. Every citation is gathered automatically in the References
section below, drawn from `references.bib`.

## Tables {#sec-tables}

Give a code chunk a `tbl-` label and a caption to make it cross-referable.
@tbl-summary is built with `knitr::kable()`.

```{r}
#| label: tbl-summary
#| tbl-cap: "First rows of the built-in mtcars dataset."

knitr::kable(head(mtcars[, 1:5]))
```

## Figures {#sec-figures}

A `fig-` label does the same for plots, so @fig-scatter can be referenced from
anywhere in the text.

```{r}
#| label: fig-scatter
#| fig-cap: "Fuel efficiency against weight in the mtcars dataset."

plot(mtcars$wt, mtcars$mpg,
     xlab = "Weight (1000 lbs)",
     ylab = "Miles per gallon",
     pch = 19)
```

## Equations {#sec-equations}

Write maths inline, such as $y = \\beta_0 + \\beta_1 x$, or as a numbered
display equation like @eq-model:

$$
y_i = \\beta_0 + \\beta_1 x_i + \\varepsilon_i
$$ {#eq-model}

## Cross-references {#sec-crossref}

References work across the document too: see @sec-tables for tables and
@sec-figures for figures. Quarto renders every `@` reference as a numbered link
automatically.
'

  # Shared manuscript trailer ----------------------------------------------------

  manuscript_tail <- "
\\singlespacing

## References

::: {#refs}
:::

## Appendix {.appendix}

"

  # Statutory declaration (appended when stat_decl = TRUE) -----------------------

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

  # Native LaTeX Title Page Snippet ----------------------------------------------
  # A single, easy-to-edit cover page. Title, subtitle, and author are filled in
  # from the function call; the date uses LaTeX's built-in \\today. (The date set
  # in _quarto.yml is separately rendered by \\maketitle on the abstract page.)

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

  quarto_code_notebook <- paste0(
    "---
title: |
  Code Template
subtitle: |
  ",
    title,
    ": ",
    subtitle,
    "
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

> See [Quarto computations with Positron](https://quarto.org/docs/get-started/computations/positron.html) for help with code and computations.

> Copy this file and rename it (e.g. `01_descriptives.qmd`) for each analysis you do.

# Setup

```{r}
#| label: setup

# To track computation time
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


# Computation Time

```{r}
#| label: computation-time

end_time <- Sys.time()

computation_time <- end_time - start_time

message(paste(\"Computation completed in:\", round(as.numeric(computation_time, units = \"secs\"), 2), \"seconds.\n\"))
```


  "
  )

  # Presentation QMD -------------------------------------------------------------

  quarto_presentation_content_default <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "subtitle: \"", subtitle, "\"\n",
    "format:\n",
    "  revealjs:\n",
    "    embed-resources: true\n",
    "---\n",
    "\n",
    "> See the [Quarto Reveal.js documentation](https://quarto.org/docs/presentations/revealjs/) for help with slide authoring.\n",
    "\n",
    "## Overview\n",
    "\n",
    "- First point\n",
    "- Second point\n"
  )

  # Project-level _quarto.yml ----------------------------------------------------

  quarto_project_yaml_content <- paste0(
    "project:\n",
    "  type: default\n",
    "\n",
    "author: \"", author_with_details, "\"\n",
    "date: last-modified\n",
    "date-format: MMMM D, YYYY\n",
    "bibliography: references.bib\n",
    "biblio-style: apsr\n",
    "link-citations: true\n",
    "\n",
    "execute:\n",
    "  echo: false\n",
    "  warning: false\n",
    "  eval: true\n",
    "  cache: true\n"
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

  # --- Create project-level _quarto.yml ---
  message("\nCreating _quarto.yml project metadata file...")
  create_file_with_content(
    file_path = "_quarto.yml",
    content = quarto_project_yaml_content,
    overwrite = overwrite
  )

  # --- Conditional File Creation ---

  # Manuscript files
  if (manuscript) {
    message("\nCreating manuscript files...")

    # Compose the manuscript from independent parts: YAML front matter (plain or
    # title-page), body (placeholder or tutorial), the shared trailer, and the
    # optional statutory declaration.
    manuscript_content <- paste0(
      if (titlepage) manuscript_yaml_titlepage else manuscript_yaml_default,
      manuscript_intro,
      if (tutorial) manuscript_body_tutorial else manuscript_body_lipsum,
      manuscript_tail,
      if (stat_decl) stat_decl_content else ""
    )

    create_file_with_content(
      file_path = "manuscript.qmd",
      content = manuscript_content,
      overwrite = overwrite
    )

    if (titlepage) {
      create_file_with_content(
        file_path = "title-page.tex",
        content = title_page_tex_content,
        overwrite = overwrite
      )
    }
  }

  # Presentation files
  if (presentation) {
    message("\nCreating presentation qmd...")
    create_file_with_content(
      file_path = "presentation.qmd",
      content = quarto_presentation_content_default,
      overwrite = overwrite
    )
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

  # code qmd template
  if (code_files) {
    message("\nCreating .qmd code template...")

    create_file_with_content(
      file_path = "code/00_code_template.qmd",
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
