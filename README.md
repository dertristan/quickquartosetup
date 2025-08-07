# project.setup

### A lightweight R package for streamlined research project setup with Quarto

**`project.setup`** is an R package that simplifies the initialization of new research projects. It offers a single, user-friendly function—`project_setup()`—to automatically generate a standardized folder structure along with essential starter files tailored to academic workflows.

Whether you're writing a manuscript, preparing a presentation, or organizing code and data, `project.setup` helps you get started quickly with a clean, consistent, and reproducible setup. The structure is based on best practices in academic research and informed by the author’s own experience in managing projects efficiently.

---

## Installation

Install the development version of the package directly from GitHub:

```
# install.packages("remotes")  # if not already installed
remotes::install_github("dertristan/project.setup")
```

---

## Usage

The main function is `project_setup()`. Calling it with a `project_name` creates a new directory with a pre-defined folder structure and optional template files.

```
library(project.setup)

# Create a new project with the default structure
project_setup(project_name = "my_new_project")

# Customize the project with manuscript and presentation templates
project_setup(
  project_name = "my_awesome_project",
  title = "The Impact of X on Y",
  manuscript = TRUE,
  presentation = TRUE
)

# Additional options let you customize the setup further—
# for example, adding a title page, including a logo, or skipping certain components.
```
