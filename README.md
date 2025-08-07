# project.setup

### A package for quick research project setup

`project.setup` is an R package designed to streamline the creation of new research projects. It provides a single function, `project_setup()`, which automates the creation of a standardized folder structure and a suite of initial files based on common academic workflows. This helps users quickly start new projects with a consistent and organized framework, including templates for manuscripts, presentations, and code notebooks.

The package is based on the personal experiences and preferences of the author, creating a reproducible setup that supports best practices in academic research.

---

## Installation

You can install the development version of the package from GitHub:

```r
# install.packages("remotes")
remotes::install_github("dertristan/project.setup")
```

---

## Usage

The primary function is `project_setup()`. Calling it with a `project_name` will create a new directory with a default structure and files.

```r
library(project.setup)

# Create a new project with default settings
project_setup(project_name = "my_new_project")

# Create a project with a specific manuscript title and presentation
project_setup(
  project_name = "my_awesome_project",
  title = "The Impact of X on Y",
  manuscript = TRUE,
  presentation = TRUE
)

# You can also use other options to customize the setup
# (e.g., disable certain file types, add a statutory declaration, etc.)
```
