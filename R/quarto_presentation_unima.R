quarto_presentation_content_uma <- paste("---
author:", author, "
date: last-modified
date-format: MMMM D, YYYY
bibliography: references.bib
biblio-style: apsr
format:
  revealjs:
    embed-resources: true
    theme: theme.scss
    slideNumber: true
    footer: ", author, "  -- {{< meta date >}} -- ", title, "
    logo: ", logo_path, "
editor: visual
preview-links: true
---

##", title, "

###", subtitle, "

![](", title_image_path, "){width=\"100%\"}
{{< meta author >}}<br>
{{< meta date >}}

# Introduction

## Motivation

- Bullet point 1
- more details @ [Quarto Revealjs Documentation](https://quarto.org/docs/presentations/revealjs/)

------------------------------------------------------------------------

## Relevance

------------------------------------------------------------------------

## Research Question

# Theory

## Prior Research

------------------------------------------------------------------------

## Theoretical Framework

------------------------------------------------------------------------

## Argument

# Research Design

------------------------------------------------------------------------

## Data

------------------------------------------------------------------------

## Methods

# Results

------------------------------------------------------------------------

## Results I

------------------------------------------------------------------------

## Results II

# Conclusion

------------------------------------------------------------------------

## Summary

------------------------------------------------------------------------

## Implications

# Thank you for your attention!

------------------------------------------------------------------------

## References
")

scss_content <- "/*-- scss:defaults --*/
$caption-background: #003056;
$main-background: white;
$main-text: #003056;
$footnote-background: #003056;
$presentation-heading-color: #003056;

/*-- scss:rules --*/
/*.reveal .slides > section > h1, .reveal .slides > section > section > h2 {
    background-color: $caption-background;
    color: $main-background;
} */

#title-slide {
  .title {
    color: #003056; /* This is the fill color for the inside of the text */
  /*  -webkit-text-stroke: 1px #DE7E50; /* This adds the border color */
  /*  text-stroke: 1px #DE7E50; /* For non-WebKit browsers */
  }

  .subtitle {
    color: #003056;
  /*  -webkit-text-stroke: 1px #DE7E50;
  /*  text-stroke: 1px #DE7E50; */
  }

  .quarto-title-author {
    color: #003056;
  /*  -webkit-text-stroke: 1px #DE7E50;
    text-stroke: 1px #DE7E50; */
  }

  .quarto-title-date {
    color: #003056;
   /* -webkit-text-stroke: 1px #DE7E50;
    text-stroke: 1px #DE7E50; */
  }
}

.reveal .slides > section > p, .reveal .slides > section > section > p {
    color: $main-text;
}

.reveal .slide-number {
    background-color: $footnote-background;
    color: $main-text;
    bottom: 14px !important;
    right: 50px !important;
    top: unset !important;
}

.reveal .footer {
    background-color: $main-background;
    color: $main-text;
}

/* Custom color for author and date */
.quarto-author, .quarto-date {
  color: #003056; /* Change to your desired color */
}

/* Custom link and list styles */
.reveal a {
  color: #DE7E50;
}
.reveal li {
  color: #003056;
}

/* Adjust the logo size */
.reveal .slide-logo {
        max-height: 4em !important;
        top: 0;
        right: 12px
      }
"
