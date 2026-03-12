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
- **Custom metadata** – Add title, subtitle, author, institution, and more right at setup.
- **Student-ready options** – Include statutory declarations and student IDs for term papers.
- **Optional title page** – Generate formal academic title pages for examination papers.
- **University of Mannheim support** – Preloaded branding and statutory declaration (currently tailored to the School of Social Sciences at the University of Mannheim, with support for other institutions planned).

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

- `manuscript.qmd` – main manuscript  
- `presentation.qmd` – linked presentation  
- `01_code.qmd` – reproducible code notebook  
- `data` folder – with raw, processed, and final subfolders  
- `references.bib` – shared bibliography  
- `images` folder – includes University logo (currently Mannheim School of Social Sciences)  
- `theme.scss` – custom styling for Reveal.js presentations

**Project structure**:

```text
my_new_project
├── code
│   └── 01_code.qmd
├── data
│   ├── 01_raw
│   ├── 02_processed
│   └── 03_final
├── images
│   ├── COPYRIGHTS.md
│   ├── uma_palace.png
│   └── uma_ss.png
├── manuscript.qmd
├── presentation.qmd
├── references.bib
├── theme.scss
└── _extensions
    ├── andrewheiss/wordcount
    └── nmfs-opensci/titlepage
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

- [Quarto wordcount extension](https://github.com/andrewheiss/quarto-wordcount) -- developed by [Andrew Heiss](https://github.com/andrewheiss)  
- [Quarto titlepages extension](https://github.com/nmfs-opensci/quarto_titlepages) -- developed by [NMFS Open Science](https://github.com/nmfs-opensci)

---

## 🛠 Development Status

`quickquartosetup` is under active development. The next major focus is simplification and tailoring for the QMIR course workflow, moving towards a minimalist package with no external extension dependencies. Planned improvements include:

### Core simplification (QMIR course focus)
- [ ] Remove Quarto extension dependencies (wordcount, titlepages) — implement title page natively
- [ ] Remove institution-specific branding (logos, university-specific files) — make templates generic and institution-agnostic
- [ ] Streamline default project structure to match the QMIR course workflow (R + Quarto + Git/GitHub in Positron)

### Manuscript & output options
- [ ] Consistent default templates for manuscript (with and without title page) and presentation
- [ ] Native title page implementation without external extension
- [ ] Improve `theme.scss` for Reveal.js presentations
- [ ] Statutory declaration as standalone, institution-agnostic template

### Metadata & configuration
- [ ] Create one central YAML metadata file for all project documents
- [ ] Language support — starting with **German** (for humanities workflows)
- [ ] Edit default PDF to include all academic metadata

### Quarto learning content
- [ ] Include commented examples for common Quarto features: citations, figures, tables, equations, cross-references
- [ ] Optional helper scripts for common tasks
- [ ] Multibib option for separate primary and secondary references
- [ ] Custom title page for students

### Technical
- [ ] Fix installation warnings
- [ ] Improve "folder already exists" handling for Quarto extensions
- [ ] Switch from manual copying of extensions to installing them from source repos (until extensions are dropped)
- [ ] Option for initializing `.Rproj` files on the fly
- [ ] Add prerequisites to the installation guide

---

## 📄 License

MIT License © Tristan Muno

---

## 📚 Citation

If you use this package in your research:

> Muno, Tristan (2025). *quickquartosetup: A lightweight R package for streamlined research project setup with Quarto*. https://github.com/dertristan/quickquartosetup
