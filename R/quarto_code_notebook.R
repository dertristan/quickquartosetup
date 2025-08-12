# Code QMD ---------------------------------------------------------------------

quarto_code_notebook <- paste0(
  "---
title: |
  Code Notebook
subtitle: |
  ", title, ": ", subtitle, "
author:
  - name: ", author, "
    email: ", mail, "
    affiliations:
      - name: ", institution, "
        department: School of Social Sciences
date: last-modified
date-format: MMMM D, YYYY
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

# Setup

```{r}
#| label: setup

# To track render duration
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


# Render Time

```{r}
#| label: render-time

end_time <- Sys.time()

rendering_time <- end_time - start_time

message(paste(\"Document rendered in:\", round(as.numeric(rendering_time, units = \"secs\"), 2), \"seconds.\n\"))
```


  "
)
