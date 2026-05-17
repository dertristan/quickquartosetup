# quickquartosetup

### A lightweight R package for fast, consistent research project setup with Quarto

**`quickquartosetup`** makes starting a new research project painless.  
With a single call to `project_setup()`, you can instantly create a clean, reproducible folder structure, complete with essential starter files designed for academic workflows.

Whether you're writing a manuscript, preparing a presentation, or organizing your analysis pipeline, `quickquartosetup` helps you work faster and stay organized. The structure follows best practices in academic research and is based on the author's experience managing multiple projects efficiently.

> **Note:** This package originated as a personal side project and is now being actively developed as a course companion for [QMIR – Quantitative Methods in International Relations and European Politics](https://github.com/qmir-2026) at the University of Mannheim. The goal is a **minimalist, dependency-light package** optimized for the workflow taught in the course: **R + Quarto + Git/GitHub in Positron**. It is intentionally kept simple and opinionated to lower the barrier for students applying course workflows to their own term papers and theses.

---

## ✨ Features

- **One command, in-place scaffolding** – Call `project_setup()` from inside your project folder and the package writes its files directly into the current working directory. Existing files (cloned dataset, README, etc.) are left untouched.
- **Built-in Quarto integration** – Generates `.qmd` files for the manuscript, presentation, and code template, plus a `references.bib` and a `.gitignore`.
- **Centralized project metadata** – A single `_quarto.yml` defines the shared author/date/bibliography/execute defaults for every document.
- **Student-ready options** – Include a bilingual (German/English) statutory declaration and a student ID footnote for term papers.
- **Native title page** – Optional `title-page.tex` included before the manuscript body. No Quarto extensions required.
- **Quarto getting-started links** – Each generated document carries a link to the relevant Quarto documentation page, so students can quickly find help.

---

## 📦 Installation

Install the development version from GitHub:

```r
# install.packages("remotes")  # if not already installed
remotes::install_github("dertristan/quickquartosetup", build_vignettes = TRUE)
```

---

## 🚀 Usage

The core function is `project_setup()`. The intended workflow: clone or `cd` into the folder where your project lives (e.g. an exam repository containing a dataset and a README), then call `project_setup()`. The function writes its scaffold into the current working directory and never touches files it didn't generate itself.

**Basic example**:

```r
library(quickquartosetup)

# From inside the project folder
project_setup()
```

This scaffolds:

- `_quarto.yml` – project-level metadata (author, date, bibliography, execute defaults) shared across documents
- `manuscript.qmd` – main manuscript
- `presentation.qmd` – minimal Reveal.js presentation
- `code/00_code_template.qmd` – copy-and-rename code template for each analysis
- `data/` – with `01_raw`, `02_processed`, and `03_final` subfolders
- `references.bib` – shared bibliography
- `.gitignore`

When `title_page = TRUE` a native `title-page.tex` snippet is added and pulled in via `include-before-body`.

**Project structure (defaults)**:

```text
.
├── _quarto.yml
├── code
│   └── 00_code_template.qmd
├── data
│   ├── 01_raw
│   ├── 02_processed
│   └── 03_final
├── manuscript.qmd
├── presentation.qmd
├── references.bib
└── .gitignore
```

The setup keeps data, code, and outputs clearly separated -- making it easy to embed computed results directly into manuscripts or presentations using [Quarto's embedding feature](https://quarto.org/docs/authoring/notebook-embed.html).

---

**Custom example**:

```r
project_setup(
  author = "Bilbo Baggins",
  title = "There and Back Again",
  subtitle = "A Hobbit's Holiday",
  student_id = 3791,
  stat_decl = TRUE
)
```

Here's a preview of the rendered manuscript and presentation based on an earlier configuration:

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
- [x] Streamline default project structure to match the QMIR course workflow (R + Quarto + Git/GitHub in Positron) — `project_setup()` now scaffolds in-place

### Manuscript & output options
- [x] Consistent default templates for manuscript (with and without title page) and presentation
- [x] Native title page implementation without external extension
- [x] Statutory declaration as standalone, institution-agnostic template

### Metadata & configuration
- [x] Create one central YAML metadata file for all project documents
- [x] Move the author field (with optional mail/student-id footnote) into `_quarto.yml`
- [ ] Language support — starting with **German** (for humanities workflows)
- [ ] Edit default PDF to include all academic metadata

### Quarto learning content
- [x] Add Quarto getting-started links to each generated file
- [x] Reframe the code notebook as a copy-and-rename template (`code/00_code_template.qmd`)
- [ ] Include commented examples for common Quarto features: citations, figures, tables, equations, cross-references
- [ ] Optional helper scripts for common tasks
- [ ] Multibib option for separate primary and secondary references

### Technical
- [ ] Fix installation warnings (license pointer, non-ASCII chars in `R/project_setup.R`, vignette artifacts)
- [ ] Update the introduction vignette to match the simplified signature
- [ ] Option for initializing `.Rproj` files on the fly
- [ ] Add prerequisites to the installation guide

---

## 📄 License

MIT License © Tristan Muno

---

## 📚 Citation

If you use this package in your research:

> Muno, Tristan (2025). *quickquartosetup: A lightweight R package for streamlined research project setup with Quarto*. https://github.com/dertristan/quickquartosetup
