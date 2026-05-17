# quickquartosetup

### A lightweight R package for fast, consistent research project setup with Quarto

**`quickquartosetup`** makes starting a new research project painless.  
With a single call to `project_setup()`, you can instantly create a clean, reproducible folder structure, complete with essential starter files designed for academic workflows.

Whether you're writing a manuscript, preparing a presentation, or organizing your analysis pipeline, `quickquartosetup` helps you work faster and stay organized. The structure follows best practices in academic research and is based on the author's experience managing multiple projects efficiently.

> **Note:** This package originated as a personal side project and is now being actively developed as a course companion for [QMIR – Quantitative Methods in International Relations and European Politics](https://github.com/qmir-2026) at the University of Mannheim. The goal is a **minimalist, dependency-light package** optimized for the workflow taught in the course: **R + Quarto + Git/GitHub in Positron**. It is intentionally kept simple and opinionated to lower the barrier for students applying course workflows to their own term papers and theses.

---

## ✨ Features

- **One command, full setup** – Create a ready-to-use project structure with `project_setup()`.
- **Built-in Quarto integration** – Automatically generates `.qmd` files for manuscripts, presentations, and notebooks.
- **Custom metadata** – Add title, subtitle, author, institution, department, and more right at setup.
- **Centralized project metadata** – A single `_quarto.yml` defines shared metadata (author, date, bibliography, execute defaults) for every document.
- **Student-ready options** – Include a bilingual (German/English) statutory declaration and student IDs for term papers.
- **Native title page** – Optional `title-page.tex` included before the manuscript body. No Quarto extensions required.
- **Customizable presentation theme** – Optional `theme.scss` for Reveal.js, with the primary color controlled by a single `theme_color` parameter.

---

## 📦 Installation

Install the development version from GitHub:

```r
# install.packages("remotes")  # if not already installed
remotes::install_github("dertristan/quickquartosetup", build_vignettes = TRUE)
```

---

## 🚀 Usage

The core function is `project_setup()`.

**Basic example**:

```r
library(quickquartosetup)

# Create a new project with default settings
project_setup(project_name = "my_new_project")
```

This creates a project folder with:

- `_quarto.yml` – project-level metadata (author, date, bibliography, execute defaults) shared across documents
- `manuscript.qmd` – main manuscript
- `presentation.qmd` – linked presentation
- `01_code.qmd` – reproducible code notebook
- `data` folder – with raw, processed, and final subfolders
- `references.bib` – shared bibliography

When `title_page = TRUE` a native `title-page.tex` snippet is added and pulled in via `include-before-body`. When `uma_style = TRUE` a `theme.scss` is generated for the Reveal.js presentation.

**Project structure (defaults)**:

```text
my_new_project
├── _quarto.yml
├── code
│   └── 01_code.qmd
├── data
│   ├── 01_raw
│   ├── 02_processed
│   └── 03_final
├── manuscript.qmd
├── presentation.qmd
└── references.bib
```

The setup keeps data, code, and outputs clearly separated -- making it easy to embed computed results directly into manuscripts or presentations using [Quarto's embedding feature](https://quarto.org/docs/authoring/notebook-embed.html).

---

**Custom example**:

```r
project_setup(
  project_name = "my_other_project",
  author = "Bilbo Baggins",
  title = "There and Back Again",
  subtitle = "A Hobbit's Holiday",
  institution = "Hobbiton Academy of Sciences",
  student_id = 3791,
  stat_decl = TRUE
)
```

Here's a preview of the rendered manuscript and presentation based on the function call above:

<p align="center">
  <a href="https://github.com/dertristan/quickquartosetup/blob/main/vignettes/my-other-project-rendered-manuscript.pdf">
    <img src="https://raw.githubusercontent.com/dertristan/quickquartosetup/main/vignettes/my-other-project-rendered-manuscript-firstpage.png" alt="Screenshot of manuscript" width="40%">
  </a>
  <a href="https://raw.githubusercontent.com/dertristan/quickquartosetup/main/vignettes/my-other-project-rendered-presentation.html">
    <img src="https://raw.githubusercontent.com/dertristan/quickquartosetup/main/vignettes/my-other-project-rendered-presentation-firstslide.png" alt="Screenshot of presentation" width="50%">
  </a>
</p>

See the vignette for an introduction and the documentation for all available arguments.

---

## 🙌 Credits

Earlier versions of the package bundled the [Quarto wordcount extension](https://github.com/andrewheiss/quarto-wordcount) by [Andrew Heiss](https://github.com/andrewheiss) and the [Quarto titlepages extension](https://github.com/nmfs-opensci/quarto_titlepages) by [NMFS Open Science](https://github.com/nmfs-opensci). The native title page now used in the package was inspired by their work.

---

## 🛠 Development Status

`quickquartosetup` is under active development. The current focus is simplification and tailoring for the QMIR course workflow.

### Core simplification (QMIR course focus)
- [x] Remove Quarto extension dependencies (wordcount, titlepages) — title page implemented natively
- [x] Remove institution-specific branding (logos, university-specific files) — templates are now generic and institution-agnostic
- [ ] Streamline default project structure to match the QMIR course workflow (R + Quarto + Git/GitHub in Positron)

### Manuscript & output options
- [x] Consistent default templates for manuscript (with and without title page) and presentation
- [x] Native title page implementation without external extension
- [ ] Improve `theme.scss` for Reveal.js presentations
- [x] Statutory declaration as standalone, institution-agnostic template

### Metadata & configuration
- [x] Create one central YAML metadata file for all project documents
- [ ] Language support — starting with **German** (for humanities workflows)
- [ ] Edit default PDF to include all academic metadata

### Quarto learning content
- [ ] Include commented examples for common Quarto features: citations, figures, tables, equations, cross-references
- [ ] Optional helper scripts for common tasks
- [ ] Multibib option for separate primary and secondary references
- [ ] Custom title page for students

### Technical
- [ ] Fix installation warnings
- [ ] Option for initializing `.Rproj` files on the fly
- [ ] Add prerequisites to the installation guide

---

## 📄 License

MIT License © Tristan Muno

---

## 📚 Citation

If you use this package in your research:

> Muno, Tristan (2025). *quickquartosetup: A lightweight R package for streamlined research project setup with Quarto*. https://github.com/dertristan/quickquartosetup
