# qqs (**Q**uick**Q**uarto**S**etup)

### A lightweight R package for fast, consistent research project setup with Quarto

**`qqs`** (short for *quick quarto setup*) makes starting a new research project painless.
With a single call to `qqs()`, you can instantly create a clean, reproducible folder structure, complete with essential starter files designed for academic workflows.

Whether you're writing a manuscript, preparing a presentation, or organizing your analysis pipeline, `qqs` helps you work faster and stay organized. The structure follows best practices in academic research and is based on the author's experience managing multiple projects efficiently.

> **Note:** This package originated as a personal side project and is now being actively developed as a course companion for [QMIR -- Quantitative Methods in International Relations and European Politics](https://github.com/qmir-2026) at the University of Mannheim. The goal is a **minimalist, dependency-light package** optimized for the workflow taught in the course: **R + Quarto + Git/GitHub in Positron**. It is intentionally kept simple and opinionated to lower the barrier for students applying course workflows to their own term papers and theses.

---

## ✨ Features

- **One command, in-place scaffolding** -- Call `qqs()` from inside your project folder and the package writes its files directly into the current working directory. Existing files (cloned dataset, README, etc.) are left untouched.
- **Built-in Quarto integration** -- Generates `.qmd` files for the manuscript, presentation, and code template, plus a `references.bib` and a `.gitignore`.
- **Centralized project metadata** -- A single `_quarto.yml` defines the shared author/date/bibliography/execute defaults for every document.
- **Student-ready options** -- Include a bilingual (German/English) statutory declaration and a student ID footnote for term papers.
- **Live Quarto tutorial** -- With `tutorial = TRUE`, the manuscript is filled with real, rendering examples (prose formatting, a table, a figure, equations, cross-references, and citations) so the scaffold doubles as a worked Quarto reference. Uses only base R + knitr -- no extra packages.
- **Native title page** -- With `titlepage = TRUE`, a single, easy-to-edit `title-page.tex` cover page is generated and the front matter is arranged as cover page → abstract → table of contents → body. No Quarto extensions required.
- **Quarto getting-started links** -- Each generated document carries a link to the relevant Quarto documentation page, so students can quickly find help.

---

## 📦 Installation

Install the development version from GitHub:

```r
# install.packages("remotes")  # if not already installed
remotes::install_github("dertristan/qqs")
```

---

## 🚀 Usage

The core function is `qqs()`. The intended workflow: clone or `cd` into the folder where your project lives (e.g. an exam repository containing a dataset and a README), then call `qqs()`. The function writes its scaffold into the current working directory and never touches files it didn't generate itself.

**Basic example**:

```r
library(qqs)

# From inside the project folder
qqs()
```

This scaffolds:

- `_quarto.yml` -- project-level metadata (author, date, bibliography, execute defaults) shared across documents
- `manuscript.qmd` -- main manuscript
- `presentation.qmd` -- minimal Reveal.js presentation
- `code/00_code_template.qmd` -- copy-and-rename code template for each analysis
- `data/` -- with `01_raw`, `02_processed`, and `03_final` subfolders
- `references.bib` -- shared bibliography
- `.gitignore`

When `titlepage = TRUE` a native `title-page.tex` cover page is added (pulled in via `include-before-body`) and the front matter renders as cover page → abstract → table of contents → body. When `tutorial = TRUE` the manuscript body is populated with live, rendering Quarto examples instead of placeholder text.

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
qqs(
  author = "Bilbo Baggins",
  title = "There and Back Again",
  subtitle = "A Hobbit's Holiday",
  student_id = 3791,
  titlepage = TRUE,   # cover page -> abstract -> TOC -> body
  tutorial = TRUE,    # fill the manuscript with live Quarto examples
  stat_decl = TRUE
)
```

See `?qqs` for the full list of arguments.

---

## Helpful Quarto Docs

- [Quarto authoring with Positron](https://quarto.org/docs/get-started/authoring/positron.html) for help with manuscript authoring.

- [Quarto computations with Positron](https://quarto.org/docs/get-started/computations/positron.html) for help with code and computations.


- [Quarto Reveal.js documentation](https://quarto.org/docs/presentations/revealjs/) for help with slide authoring.


---

## 📄 License

MIT License © Tristan Muno

---

## 📚 Citation

If you use this package in your research:

> Muno, Tristan (2026). *qqs: A lightweight R package for streamlined research project setup with Quarto*. https://github.com/dertristan/qqs
