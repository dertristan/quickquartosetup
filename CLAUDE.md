# qqs — CLAUDE.md

## What this project is

`qqs` (short for *quick quarto setup*) is a lightweight R package (v0.0.0.9000, MIT) authored by Tristan Muno. Its single purpose: one call to `qqs()` scaffolds an opinionated set of Quarto template files into the **current working directory** — leaving any pre-existing files (cloned datasets, READMEs, etc.) untouched.

It started as a personal tool and is now being refined as a course companion for **QMIR – Quantitative Methods in International Relations and European Politics** at the University of Mannheim. The guiding design principle is **minimalism and low dependencies**, to keep it easy for students using R + Quarto + Git/GitHub in Positron.

The intended workflow: a student clones an exam repository (already containing a dataset and a README), navigates into the folder, and calls `qqs()` to scaffold their working files in place.

---

## Repository layout

```
qqs/
├── R/
│   ├── qqs.R                  # The one public function — all logic lives here
│   └── qqs-package.R          # Package-level roxygen docs
├── dev-guide.qmd              # Git/GitHub branching reference (course material)
├── DESCRIPTION
├── LICENSE.md
└── README.md
```

Note: `inst/` and `vignettes/` have been removed. The package no longer ships any Quarto extensions, branded image assets, or vignettes.

---

## Core function: `qqs()`

**File**: `R/qqs.R`

Single exported function. All template content is defined as inline strings inside the function body (no separate template files).

### Key parameters

| Parameter | Default | Purpose |
|---|---|---|
| `manuscript` | `TRUE` | Generate `manuscript.qmd` (plain `pdf` format) |
| `presentation` | `TRUE` | Generate a minimal `presentation.qmd` (Reveal.js) |
| `code_files` | `TRUE` | Generate `code/00_code_template.qmd` (HTML, copy-and-rename template) |
| `data_folders` | `TRUE` | Create `data/01_raw`, `02_processed`, `03_final` |
| `gitignore` | `TRUE` | Generate `.gitignore` |
| `author` | system username | Populates `_quarto.yml` (with optional footnote from `mail` / `student_id`) |
| `mail` | placeholder | When non-default, added as `Mail: …` footnote on the author |
| `student_id` | `"1234567"` | When non-default, added as `Student ID: …` footnote on the author |
| `title` / `subtitle` | `"Untitled Project"` / `"A great project"` | YAML metadata |
| `title_page` | `FALSE` | Generate `title-page.tex` and include it via `include-before-body` |
| `stat_decl` | `FALSE` | Append German/English statutory declaration |
| `overwrite` | `TRUE` | Whether to overwrite files `qqs()` itself generates (pre-existing user files are never touched) |

### Generated project structure

The scaffold is written into the **current working directory** — there is no wrapping project folder.

```
.
├── _quarto.yml                    # Project-level metadata shared across documents (author, date, bibliography, execute defaults)
├── code/00_code_template.qmd      # HTML code template — students copy and rename for each analysis
├── data/01_raw/
├── data/02_processed/
├── data/03_final/
├── manuscript.qmd                 # Plain PDF manuscript (default or titlepage variant)
├── title-page.tex                 # Only when title_page = TRUE
├── presentation.qmd               # Minimal Reveal.js deck (auto title slide + one content slide)
├── references.bib                 # BibTeX starter entries
└── .gitignore
```

### Manuscript variants (chosen by flag combination)

- Default: plain `pdf` format; the author (with its optional `Mail` / `Student ID` footnote) is inherited from `_quarto.yml`
- `title_page = TRUE`: plain `pdf` format with `include-before-body: title-page.tex`; the title page is rendered from a small native LaTeX snippet (no Quarto extensions involved)
- Any + `stat_decl = TRUE`: appends bilingual (German/English) statutory declaration

Shared YAML metadata (author with optional footnote, date, bibliography, biblio-style, link-citations, execute defaults) lives in `_quarto.yml` and is inherited by every document. Per-file YAML now only carries format-specific keys and intentional overrides (e.g. the code notebook's `echo: true`). Note: the LaTeX `^[…]` footnote syntax in the author string does not render cleanly in the HTML code template — this is an accepted trade-off, since the PDF manuscript is the primary document.

### Internal helpers (defined inside `qqs()`)

- `create_folder(folder_path)` — idempotent `dir.create(recursive = TRUE)`
- `create_file_with_content(file_path, content, overwrite)` — writes text with overwrite logic

---

## Active development direction

Rounds one through three of the QMIR-course simplification roadmap are complete:

1. ~~**Remove Quarto extension dependencies**~~ — title page is now native; wordcount has been removed
2. ~~**Make templates institution-agnostic**~~ — UMA branding and logos removed
3. ~~**Central YAML metadata file**~~ — shared keys live in `_quarto.yml`
4. ~~**In-place scaffolding**~~ — `qqs()` writes into the current working directory rather than creating a subfolder
5. ~~**Slim the signature**~~ — removed `project_name`, `target_path`, `institution`, `department`, `uma_style`, `theme_color`; stripped `theme.scss` and the UMA presentation variant
6. ~~**Author in `_quarto.yml`**~~ — the author field (with optional mail/student-id footnote) is no longer duplicated in `manuscript.qmd`
7. ~~**Reusable code template**~~ — `code/01_code.qmd` renamed to `code/00_code_template.qmd` with copy-and-rename guidance baked in
8. ~~**Quarto getting-started links**~~ — every generated document carries a visible link to the relevant Quarto docs page
9. ~~**Rename to `qqs`**~~ — package is now `qqs`, the single exported function is `qqs()`, the vignette has been removed

Remaining work:

10. **German language support**
11. **Commented Quarto examples** in generated files (citations, figures, tables, cross-references)
12. **Optional helper scripts**
13. Fix installation warnings (license pointer in DESCRIPTION, non-ASCII in `R/qqs.R`)

---

## Development notes

- No test suite yet (`testthat` not listed in DESCRIPTION)
- No runtime or vignette dependencies — `Suggests` is empty
- Use `devtools::load_all()` to test changes without reinstalling
- Use `devtools::check()` before merging
- The `dev-guide.qmd` is a Git branching guide for course students, not package documentation

---

## Implementation strategies for the QMIR course roadmap

These are design notes for translating the remaining roadmap items into concrete changes. Items 1–9 have been completed; see the commit history for what landed.

---

### 10. German language support

**The problem**: Quarto outputs labels like "Figure", "Table", "References" in English by default.

**Strategy**:
- Add a `language = "en"` parameter (options: `"en"`, `"de"`)
- When `"de"`, inject `lang: de` into the YAML of generated files — Quarto natively translates labels to German when `lang: de` is set
- Section headings in the manuscript template can be switched: Introduction → Einleitung, Theory → Theorie, etc. — store these as a named list in a small helper at the top of `qqs()`
- The statutory declaration is already bilingual; no change needed there
- Keep English as the only supported option for now; add `"de"` as the first extension

---

### 11. Commented Quarto examples in generated files

**The problem**: Students new to Quarto don't know how to add citations, figures, tables, or cross-references in the manuscript template.

**Strategy**:
- Add a `quarto_examples = FALSE` parameter
- When `TRUE`, append a commented-out `## Examples` section to the manuscript template with annotated snippets:
  - Citation: `[@lastname2023]`
  - Figure with cross-ref: `` ```{r fig-name} `` + `@fig-name`
  - Table with cross-ref (via `knitr::kable()` or `gt`)
  - Inline equation: `$y = \beta_0 + \beta_1 x$`
  - Display equation: `$$...$$`
  - Section cross-ref: `@sec-introduction`
- Keep examples as a separate string constant (e.g., `quarto_examples_content`) and `paste0()` it onto whichever manuscript variant was selected — same pattern already used for `stat_decl_content`
- This keeps the default output clean while making it easy to opt in

---

### 12. Optional helper scripts

**The problem**: Students repeatedly write the same boilerplate for loading packages, setting paths with `here`, and checking/installing dependencies.

**Strategy**:
- Add a `helper_scripts = FALSE` parameter
- When `TRUE`, generate `R/helpers.R` (or `code/helpers.R`) with:
  - A `load_packages()` wrapper that checks, installs, and loads a vector of packages (the pattern already used in the setup chunk of `00_code_template.qmd`, extracted as a reusable function)
  - A commented block showing `here::here()` usage for cross-platform paths
- Keep the script minimal — 30–40 lines — and fully commented for students

---

### Architectural consideration: template strings vs. file-based templates

**Current approach**: all content lives as `paste0()` multi-line strings inside `qqs()`. This is self-contained but still makes the function ~700 lines and harder to edit than a plain `.qmd` file.

**Alternative worth considering**: move templates to `inst/templates/*.qmd` files with `{{{placeholder}}}` markers and use `whisker::whisker.render()` for substitution (or just `gsub()` without adding a dependency). This would:
- Make template editing much easier (edit a real `.qmd` file, not a string inside R code)
- Allow templates to be previewed independently
- Reduce the function body by ~80%

**Trade-off**: adds either a `whisker` dependency or a custom `gsub()`-based renderer. Given the QMIR minimalism goal, a dependency-free `gsub()` approach on clearly named `{{variable}}` tokens is a reasonable middle ground and worth doing before the template count grows further.
