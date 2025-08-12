# PDF Manuscript with Title Page and Logo --------------------------------------

quarto_manuscript_content_titlepage_logo <- paste0(
  "---
title: |
  ", title, "
subtitle: |
  ", subtitle, "
abstract: |
  You can add an abstract here.
author:
  - name: ", author_with_id, "
    email: ", mail, "
    affiliations:
      - name: ", institution, "
        department: School of Social Sciences
thanks: |
   You can add acknowledgements here. Wordcount: {{< words-body >}}.
date: last-modified
date-format: MMMM D, YYYY
format:
  titlepage-pdf:
    citeproc: false
    filters:
      - at: pre-quarto
        path: _extensions/andrewheiss/wordcount/citeproc.lua
      - at: pre-quarto
        path: _extensions/andrewheiss/wordcount/wordcount.lua
    titlepage: academic
    titlepage-logo: ", logo_path, "
    toc: false
    include-in-header:
      text: |
        \\usepackage{setspace}
        \\setlength{\\parindent}{15pt}
execute:
  echo: false
  warning: false
  eval: true
  include: true
  cache: true
bibliography: references.bib
biblio-style: apsr
link-citations: true
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

## Introduction {#sec-introduction}

{{< lipsum 2 >}}

## Theory {#sec-theory}

{{< lipsum 2 >}}

## Research Design {#sec-design}

{{< lipsum 2 >}}

## Empirical Analysis {#sec-analysis}

{{< lipsum 2 >}}

## Conclusion {#sec-conclusion}

{{< lipsum 2 >}}

\\singlespacing

## References

::: {#refs}
:::

## Appendix {.appendix}

"
)
