#' @title Clean references.
#'
#' @description Remove LaTeX tags and shorten authors names in YAML references
#'
#' @param files Filename(s) of references in the YAML format to be cleaned
#'
#' @export
#'
#' @details The conversion from BibTeX to YAML (using pandoc-citeproc called by
#' \code{\link[refR]{getRefs}} or \code{\link[refR]{bib2yaml}}) removes a lot of
#' LaTeX tags. But this process is not perfect. The function \code{cleanRefs}
#' cleans the remaining special characters ('exotic' accents and other LaTeX).
#' Moreover information in BibTeX is not uniform for authors, so this function
#' also clean author names (detection of family names with particle, removal of
#' uppercase except for each first letter of family and given names, and shorten
#' given name). Note that journal, booktitle and article/chapter title are also
#' not uniform but this information comes from Scopus that is more standardize
#' than Crossref.
#' Finally we have to mention that cleaned YAML files erase their previous versions
#' (this function does not return any object).
#' If errors occur and if you still have BibTeX, you can use the function
#' \code{\link[refR]{bib2yaml}} to regenerate the erased YAML file.
#'
#' @seealso \code{\link[refR]{getRefs}}, \code{\link[refR]{bib2yaml}}
#'
#' @examples
#' # Coming soon...

cleanRefs <- function(files) {

    owarn <- options()$warn
    options(warn = -1)
    on.exit(options(warn = owarn))

    if (missing(files)) {
        stop("Please provides at least one filename (BiBTeX or YAML).")
    }
    missing_au <- NULL
    missing_ti <- NULL

    cat("\n-------------------------------------------------\n")
    cat(paste0("\n       >>>     Cleaning YAML files     <<<       \n"))
    cat("\n-------------------------------------------------\n")
    cat("\n")
    for (i in seq_along(files)) {
        ref <- try(readLines(files[i]), silent = TRUE)
        if (class(ref) == "try-error") {
            stop(paste0("Unable to find/read ", files[i]))
        } else {
            cat(paste0("\r   [] Cleaned citations - ", i))
            pos <- grep("^  title:", ref)
            if (length(pos) == 0) {
                missing_ti <- c(missing_ti, files[i])
            }
            pos <- grep("family:|given:|dropping-particle:|title:|container-title:|volume:",
                ref)
            if (length(pos)) {
                for (j in seq_along(pos)) {
                  ref[pos[j]] <- gsub("\\\\textparagraph", "\u00f6", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\textcopyright", "\u00e9", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\guillemotleft", "\u00eb", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\cyrchar\\\\cyryo", "\u00eb", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\dh", "d", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\textquotesingle", "'", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\\\^", "-", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\TH", "TH", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\th", "th", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\\\[", "(", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\\\]", ")", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\textdagger|\u00c3", "", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\textasciiacute", "", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\(.)\\{([[:alpha:]])\\}", "\\2", ref[pos[j]])
                  ref[pos[j]] <- gsub("\\\\_", " ", ref[pos[j]])
                }
            }

            pos1 <- grep("^  author:", ref)
            if (length(pos1)) {
                pos2 <- grep("^  [[:alpha:]]{1,}:|^\\.\\.\\.", ref)
                pos2 <- pos2[which(pos2 > pos1)][1]
                au <- ref[(pos1 + 1):(pos2 - 1)]
                family <- grep("family:", au)
                authors <- NULL
                for (j in 1:length(family)) {
                  au_fam <- au[family[j]]
                  item <- au[family[j] + 1]
                  if (length(grep("given:", item)) == 1) {
                    au_giv <- au[family[j] + 1]
                    item <- au[family[j] + 2]
                    if (length(grep("dropping-particle:", item)) == 1) {
                      au_drp <- au[family[j] + 2]
                    } else {
                      au_drp <- ""
                    }
                  } else {
                    if (length(grep("dropping-particle:", item)) == 1) {
                      au_giv <- au[family[j] + 1]
                      au_drp <- ""
                    } else {
                      au_giv <- ""
                      au_drp <- ""
                    }
                  }
                  authors[j] <- paste0(c(au_giv, au_drp, au_fam), collapse = " ")
                  authors[j] <- gsub("  - family: |    given: |    dropping-particle: ",
                    "", authors[j])
                  authors[j] <- gsub("^ ", "", authors[j])
                }
                authors <- paste0(authors, collapse = " and ")
                authors <- gsub("\\.", "\\. ", authors)
                authors <- gsub(" -|- | - ", "-", authors)
                authors <- gsub("[[:space:]]+", " ", authors)
                authors <- gsub("\\(|\\)|\\[|\\]", "", authors)
                authors <- gsub("'(.)\\. '", "\\1.", authors)
                authors <- gsub("'(.)'", "\\1", authors)
                aut <- authors
                aut <- strsplit(aut, " and | AND ")[[1]]
                au_family <- NULL
                au_given <- NULL
                au_drop <- NULL
                for (j in 1:length(aut)) {
                  xxx <- strsplit(aut[j], " ")[[1]]
                  xxx <- xxx[which(xxx != "")]
                  family <- xxx[length(xxx)]
                  if (length(grep("-", family)) > 0) {
                    subs <- strsplit(family, "-")[[1]]
                    family <- NULL
                    for (k in 1:length(subs)) {
                      family <- c(family, paste0(toupper(substr(subs[k], 1, 1)),
                        tolower(substr(subs[k], 2, nchar(subs[k])))))
                    }
                    family <- paste0(family, collapse = "-")
                  } else {
                    family <- paste0(toupper(substr(family, 1, 1)), tolower(substr(family,
                      2, nchar(family))))
                  }
                  if (length(grep("'", family)) > 0) {
                    yyy <- strsplit(family, "'")[[1]]
                    family <- yyy[1]
                    for (k in 2:length(yyy)) {
                      family <- c(family, paste0(toupper(substr(yyy[k], 1, 1)), substr(yyy[k],
                        2, nchar(yyy[k]))))
                    }
                    family <- paste0(family, collapse = "'")
                  }
                  if (length(grep("Mc", family)) > 0) {
                    yyy <- strsplit(family, "Mc")[[1]]
                    family <- yyy[1]
                    for (k in 2:length(yyy)) {
                      family <- c(family, paste0(toupper(substr(yyy[k], 1, 1)), substr(yyy[k],
                        2, nchar(yyy[k]))))
                    }
                    family <- paste0(family, collapse = "Mc")
                  }
                  particles <- c("de", "d'", "du", "des", "of", "da", "dall'", "degli",
                    "dei", "del", "dell'", "della", "lo", "san", "am", "an", "auf",
                    "aus", "der", "im", "von", "und", "zu", "zum", "zur", "del",
                    "los", "las", "den", "ten", "ter", "te", "van", "vanden", "vander",
                    "das", "do", "dos", "af", "av")
                  pos <- which(tolower(xxx) %in% particles)
                  if (length(pos) > 0) {
                    part <- NULL
                    for (k in 1:length(pos)) {
                      part <- c(part, tolower(xxx[pos[k]]))
                    }
                    drp <- paste0(part, collapse = " ")
                  } else {
                    drp <- ""
                  }
                  xxx <- xxx[-c(pos, length(xxx))]
                  if (length(xxx) > 0) {
                    given <- NULL
                    for (k in 1:length(xxx)) {
                      pos <- grep("-", xxx[k])
                      if (length(pos) > 0) {
                        yyy <- strsplit(xxx[k], "-")[[1]]
                        zzz <- NULL
                        for (z in seq_along(yyy)) {
                          if (!(toupper(substr(yyy[z], 1, 1)) %in% LETTERS)) {
                            if ((toupper(substr(yyy[z], 2, 2)) %in% LETTERS)) {
                              zzz <- c(zzz, toupper(substr(yyy[z], 2, 2)), ".-")
                            } else {
                              zzz <- c(zzz, toupper(substr(yyy[z], 1, 1)), ".-")
                            }
                          } else {
                            zzz <- c(zzz, toupper(substr(yyy[z], 1, 1)), ".-")
                          }
                        }
                        zzz <- paste0(zzz, collapse = "")
                        given[k] <- gsub("-$", "", zzz)
                      } else {
                        if (!(toupper(substr(xxx[k], 1, 1)) %in% LETTERS)) {
                          if ((toupper(substr(xxx[k], 2, 2)) %in% LETTERS)) {
                            zzz <- c(toupper(substr(xxx[k], 2, 2)), ".")
                          } else {
                            zzz <- c(toupper(substr(xxx[k], 1, 1)), ".")
                          }
                        } else {
                          zzz <- c(toupper(substr(xxx[k], 1, 1)), ".")
                        }
                        given[k] <- paste0(zzz, collapse = "")
                      }
                    }
                    given <- paste0(given, collapse = " ")
                  } else {
                    given <- ""
                  }
                  au_family[j] <- family
                  au_given[j] <- given
                  au_drop[j] <- drp
                }
                authors <- NULL
                for (j in 1:length(au_family)) {
                  authors <- c(authors, paste0("  - family: ", au_family[j]))
                  authors <- c(authors, paste0("    given: ", au_given[j]))
                  authors <- c(authors, paste0("    dropping-particle: ", au_drop[j]))
                }
                pos <- grep("^    dropping-particle: $", authors)
                if (length(pos) > 0) {
                  authors <- authors[-pos]
                }
                pos <- grep("^    given: $", authors)
                if (length(pos) > 0) {
                  authors <- authors[-pos]
                }
                ref <- c(ref[1:pos1], authors, ref[pos2:length(ref)])
                ref <- paste0(c(ref, "\n"), collapse = "\n")
                ref <- gsub("\\n\\n", "\n", ref)
                cat(ref, file = files[i])
            } else {
                missing_au <- c(missing_au, files[i])
            }
        }
    }
    if (length(missing_au) > 0) {
        cat(paste0("\n\n   [] The following files do not contain authors:\n\t", paste0(missing_au,
            collapse = "\n\t"), "\n"))
    }
    if (length(missing_ti) > 0) {
        cat(paste0("\n[] The following files do not contain title:\n\t", paste0(missing_ti,
            collapse = "\n\t"), "\n"))
    }
    cat("\n-------------------------------------------------\n\n")


    invisible()
}
