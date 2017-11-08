# Overview

The package **refR** retrieves author references from Scopus Search and Crossref APIs and returns references as BibTeX files (function `getRefs()`). It also allows to convert BibTeX files in more readable formats (i.e. YAML and JSON) by calling the software pandoc-citeproc (functions `getRefs()`, `bib2yaml()` and `bib2json()`). Informations in references files can be cleaned using the function `cleanRefs()`. This function translates special characters (accented characters and other LaTeX tags) and also cleans author names (detection of particle in family names, removal of uppercase except for each first letter of family and given names, and shorten given name).

[![Build Status](https://travis-ci.org/inSileco/refR.svg?branch=master)](https://travis-ci.org/inSileco/refR) [![](https://img.shields.io/badge/licence-GPLv3-8f10cb.svg)](http://www.gnu.org/licenses/gpl.html)

# System requirements

The **refR** package works well on both Unix and Windows platforms. This only requirement is that user has installed the wonderful software [**Pandoc**](https://pandoc.org/), the swiss-army knife of the documents formats conversion. Do not forget to also install **pandoc-citeproc**.

# Installation

To install the package **refR** from GitHub, first install the package [**devtools**](http://cran.r-project.org/web/packages/devtools/index.html) from the CRAN.

```r
# Install the < devtools > package
install.packages("devtools", dependencies = TRUE)

# Load the < devtools > package
library(devtools)

```

Then install the **refR** package:

```r
# Install the < refR > package from GitHub
devtools::install_github("inSileco/refR", build_vignettes = TRUE)

# Load the < refR > package
library(refR)
```

# Getting started

Some useful command lines to get started:

```r
# List the content (objects and functions) of the < refR > package
ls("package:refR")

# Open the < refR > package home page
help(package = "refR")

# Open the help file of a specific function
help(getRefs)

# Open the vignette (coming soon)
# browseVignettes(package = "refR")
```

# Usage and workflow

Coming soon...


# To do list

-   \[ \] Add _JSON_ and _BibTeX_ formats in `cleanRefs()`
-   \[ \] Add a function `bib2json()`
-   \[ \] Add a function `json2bib()`
-   \[ \] Add a function `yaml2bib()`
-   \[ \] Add a function `makeBiblio()`
-   \[ \] Make a connexion with [**rscimap**](https://github.com/ahasverus/rscimap)
-   \[ \] Write a vignette with a complete working example
-   \[ \] Write a post (redundant with the vignette?)
