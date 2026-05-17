# quickquartosetup — CLAUDE.md

## What this project is

`quickquartosetup` is a lightweight R package (v0.0.0.9000, MIT) authored by Tristan Muno. Its single purpose: one call to `project_setup()` spins up a complete, opinionated research project folder with Quarto template files ready to render.

It started as a personal tool and is now being refined as a course companion for **QMIR – Quantitative Methods in International Relations and European Politics** at the University of Mannheim. The guiding design principle is **minimalism and low dependencies**, to keep it easy for students using R + Quarto + Git/GitHub in Positron.

---

## Repository layout

```
quickquartosetup/
├── R/
│   ├── project_setup.R          # The one public function — all logic lives here
│   └── quickquartosetup-package.R  # Package-level roxygen docs
├── vignettes/
│   ├── introduction.Rmd         # Package vignette (out of date: still references old extensions)
│   └── my_*/                    # Pre-rendered example projects (from vignette code)
├── dev-guide.qmd                # Git/GitHub branching reference (course material)
├── DESCRIPTION
├── LICENSE.md
└── README.md
```

Note: `inst/` has been removed. The package no longer ships any Quarto extensions or branded image assets.

---

## Core function: `project_setup()`

**File**: `R/project_setup.R`

Single exported function. All template content is defined as inline strings inside the function body (no separate template files).

### Key parameters

| Parameter | Default | Purpose |
|---|---|---|
| `project_name` | `""` (required) | Name of the new project folder |
| `target_path` | `"."` | Where to create the project |
| `manuscript` | `TRUE` | Generate `manuscript.qmd` (plain `pdf` format) |
| `presentation` | `TRUE` | Generate `presentation.qmd` (Reveal.js) |
| `code_files` | `TRUE` | Generate `code/01_code.qmd` (HTML notebook) |
| `data_folders` | `TRUE` | Create `data/01_raw`, `02_processed`, `03_final` |
| `gitignore` | `TRUE` | Generate `.gitignore` |
| `author` | system username | Populates project-level YAML metadata |
| `institution` | `"Your Institution"` | Manuscript/code YAML |
| `department` | `NULL` | Adds department line to code-notebook affiliations and the native title page when set |
| `mail` | placeholder | Manuscript/code YAML |
| `student_id` | `"1234567"` | Added to author line if non-default |
| `title` / `subtitle` | `"Untitled Project"` / `"A great project"` | YAML metadata |
| `title_page` | `FALSE` | Generate `title-page.tex` and include it via `include-before-body` |
| `stat_decl` | `FALSE` | Append German/English statutory declaration |
| `uma_style` | `FALSE` | Generate `theme.scss` and apply it to the Reveal.js presentation |
| `theme_color` | `"#333333"` | Primary color injected into `theme.scss` (applies when `uma_style = TRUE`) |
| `overwrite` | `TRUE` | Whether to overwrite existing files |

### Generated project structure

```
project_name/
├── _quarto.yml             # Project-level metadata shared across documents
├── code/01_code.qmd        # HTML notebook (file-level echo: true override)
├── data/01_raw/
├── data/02_processed/
├── data/03_final/
├── manuscript.qmd          # Plain PDF manuscript (default or titlepage variant)
├── title-page.tex          # Only when title_page = TRUE
├── presentation.qmd        # Reveal.js slides
├── theme.scss              # Only when uma_style = TRUE; uses theme_color
├── references.bib          # BibTeX starter entries
└── .gitignore
```

### Manuscript variants (chosen by flag combination)

- Default: plain `pdf` format with `author_with_details` footnote (institution/mail/ID)
- `title_page = TRUE`: plain `pdf` format with `include-before-body: title-page.tex`; the title page is rendered from a small native LaTeX snippet (no Quarto extensions involved)
- Any + `stat_decl = TRUE`: appends bilingual (German/English) statutory declaration

Shared YAML metadata (author, date, bibliography, biblio-style, link-citations, execute defaults) lives in `_quarto.yml` and is inherited by every document. Per-file YAML now only carries format-specific keys and intentional overrides (e.g. the manuscript's `author_with_details` footnote and the code notebook's `echo: true`).

### Internal helpers (defined inside `project_setup()`)

- `create_folder(folder_path)` — idempotent `dir.create(recursive = TRUE)`
- `create_file_with_content(file_path, content, overwrite)` — writes text with overwrite logic
- `copy_items(source_paths, dest_folder, overwrite)` — copies files/folders from `inst/`

---

## Active development direction

Items 1–3 of the QMIR-course simplification roadmap are complete:

1. ~~**Remove Quarto extension dependencies**~~ — title page is now native; wordcount has been removed
2. ~~**Make templates institution-agnostic**~~ — UMA branding and logos removed; `department` and `theme_color` parameters added
3. ~~**Central YAML metadata file**~~ — shared keys live in `_quarto.yml`

Remaining work:

4. **German language support**
5. **Commented Quarto examples** in generated files (citations, figures, tables, cross-references)
6. **Optional helper scripts**
7. Fix installation warnings (license pointer in DESCRIPTION, non-ASCII in `R/project_setup.R`, pre-rendered vignette outputs)

---

## Development notes

- No test suite yet (`testthat` not listed in DESCRIPTION)
- No external runtime dependencies — only `knitr` and `rmarkdown` in `Suggests` (for vignettes)
- Use `devtools::load_all()` to test changes without reinstalling
- Use `devtools::check()` before merging
- The `dev-guide.qmd` is a Git branching guide for course students, not package documentation

---

## Implementation strategies for the QMIR course roadmap

These are design notes for translating the remaining roadmap items into concrete changes. Items 1–3 have been completed; see the commit history (`Remove wordcount and titlepage extension dependencies`, `Make templates institution-agnostic`, `Centralize shared YAML metadata into _quarto.yml`) for what landed.

---

### 4. German language support

**The problem**: Quarto outputs labels like "Figure", "Table", "References" in English by default.

**Strategy**:
- Add a `language = "en"` parameter (options: `"en"`, `"de"`)
- When `"de"`, inject `lang: de` into the YAML of generated files — Quarto natively translates labels to German when `lang: de` is set
- Section headings in the manuscript template can be switched: Introduction → Einleitung, Theory → Theorie, etc. — store these as a named list in a small helper at the top of `project_setup()`
- The statutory declaration is already bilingual; no change needed there
- Keep English as the only supported option for now; add `"de"` as the first extension

---

### 5. Commented Quarto examples in generated files

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

### 6. Optional helper scripts

**The problem**: Students repeatedly write the same boilerplate for loading packages, setting paths with `here`, and checking/installing dependencies.

**Strategy**:
- Add a `helper_scripts = FALSE` parameter
- When `TRUE`, generate `R/helpers.R` (or `code/helpers.R`) with:
  - A `load_packages()` wrapper that checks, installs, and loads a vector of packages (the pattern already used in `01_code.qmd`'s setup chunk, extracted as a reusable function)
  - A commented block showing `here::here()` usage for cross-platform paths
- Keep the script minimal — 30–40 lines — and fully commented for students

---

### Architectural consideration: template strings vs. file-based templates

**Current approach**: all content lives as `paste0()` multi-line strings inside `project_setup()`. This is self-contained but makes the function ~1,200 lines and hard to read/edit.

**Alternative worth considering**: move templates to `inst/templates/*.qmd` files with `{{{placeholder}}}` markers and use `whisker::whisker.render()` for substitution (or just `gsub()` without adding a dependency). This would:
- Make template editing much easier (edit a real `.qmd` file, not a string inside R code)
- Allow templates to be previewed independently
- Reduce the function body by ~80%

**Trade-off**: adds either a `whisker` dependency or a custom `gsub()`-based renderer. Given the QMIR minimalism goal, a dependency-free `gsub()` approach on clearly named `{{variable}}` tokens is a reasonable middle ground and worth doing before the template count grows further.
