refR <img src="man/figures/refR-sticker.png" height="120" align="right"/>
=========================================================================

[![Build Status](https://travis-ci.org/inSileco/refR.svg?branch=master)](https://travis-ci.org/inSileco/refR)
[![Build status](https://ci.appveyor.com/api/projects/status/91l9qdaro5uvtak0?svg=true)](https://ci.appveyor.com/project/KevCaz/refr)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)


Overview
--------

The package **refR** retrieves author references from [Scopus Search](https://dev.elsevier.com/api_key_settings.html) and [Crossref](https://www.crossref.org/services/metadata-delivery/rest-api/) APIs and returns references as BibTeX files (function `getRefs()`). It also allows to convert BibTeX files in more readable formats (i.e. YAML and JSON) by calling the software pandoc-citeproc (functions `getRefs()`, `bib2yaml()` and `bib2json()`). Informations in references files can be cleaned using the function `cleanRefs()`. This function translates special characters (accented characters and other LaTeX tags) and also cleans author names (detection of particle in family names, removal of uppercase except for each first letter of family and given names, and shorten given name).


System requirements
-------------------

The **refR** package works well on both Unix and Windows platforms. This only requirement is that user has to install the wonderful software [**Pandoc**](https://pandoc.org/), the swiss-army knife of the documents formats conversion. Do not forget to also install [pandoc-citeproc](https://github.com/jgm/pandoc-citeproc).

Installation
------------

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

Getting started
--------

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

Usage and workflow
-----------------

```r
# Create a folder to store references
path <- "references/"
dir.create(path, showWarnings = FALSE)

# Scopus Search API Key (get freely your own) https://blog.scopus.com/topics/api
api_key   <- "9zz9z999zz99z9999zz999zz999zz999"

# Author Scopus ID
author_id <- "99999999999"

# Retrieve references from Scopus and Crossref
refs <- refR::getRefs(
  api_key   = api_key,
  author_id = author_id,
  date      = NULL,
  sort      = "pubyear",
  sleep     = 5,
  folder    = path,
  format    = c("bibtex", "yaml"),
  erase     = FALSE
)

# Print the first reference (BibTeX) in memory
cat(refs[[1]])

# List all exported YAMLs
yamls <- list.files(
  path       = path,
  pattern    = "\\.yml$",
  full.names = TRUE
)

# Import the first YAML
(ref <- yaml.load_file(yamls[1]))

# Clean all YAML references
refR::cleanRefs(files = yamls)

# Re-import the first YAML (cleaned)
(ref <- yaml.load_file(yamls[1]))

# Make bibliography
# Coming soon...

# See also
?bib2yaml
```



Bibliography operations
=======================

Find reference and extract bibliography
---------------------------------------

```R
R> findRef(text='First \\cite{Pimm2000}, second \\Citep{May1972}')
       key freq
1  May1972    1
2 Pimm2000    1
```

```R
R> findRef(text='First @Pimm2000, second @May1972, third [@May1972]', markdown=TRUE)
       key freq
1  May1972    1
2 Pimm2000    1
```


```R
R> file.name <- system.file('Bib', 'RJC.bib', package='RefManageR')
R> bib <- RefManageR::ReadBib(file.name)
R> txt <- 'First \\cite{kim1995estimating}, second \\Citep{fu2006statistical}'
R> tfile <- tempfile(fileext = '.bib')
R> extractBib(bib, txt)
$keys
[1] "fu2006statistical" "kim1995estimating"

$refs
[1] W. J. Fu, J. Hu, T. Spencer, et al. “Statistical models in assessing
fold change of gene expression in real-time RT-PCR experiments”. In:
_Computational biology and chemistry_ 30.1 (2006), pp. 21-26.

[2] M. Y. Kim, B. S. Pasternack, R. J. Carroll, et al. “Estimating the
reliability of an exposure variable in the presence of confounders”. In:
_Statistics in medicine_ 14.13 (1995), pp. 1437-1446.
```


To do list
--------

-   \[ \] Add _JSON_ and _BibTeX_ formats in `cleanRefs()`
-   \[ \] Add a function `bib2json()`
-   \[ \] Add a function `json2bib()`
-   \[ \] Add a function `yaml2bib()`
-   \[ \] Add a function `makeBiblio()`
-   \[ \] Make a connexion with [**rscimap**](https://github.com/ahasverus/rscimap)
-   \[ \] Write a vignette with a complete working example





