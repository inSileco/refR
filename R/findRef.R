#' Find citation tags.
#'
#' Finds Markdown and Latex citation tags in a character string or a file.
#'
#' @param text a character string.
#' @param con a connection object or a character string that stands for a path,
#' ignored if `text` is used.
#' @param markdown a logical. If `TRUE`,  Markdown's citation tags are sought.
#'
#' @return
#' A data frame with the citation keys and their frequency.
#'
#' @export
#' @examples
#' test <- findRef(text='First \\cite{Pimm2000}, second \\Citep{May1972}')


findRef <- function(text, con, markdown = FALSE) {
    ##---
    if (missing(text)) {
        if (missing(con)) {
            stop("Either 'text' or 'con' must be specified.")
        } else {
            lsfile <- readLines(con)
            text <- paste(unlist(lsfile), collapse = "")
        }
    }
    if (!markdown)
        pat <- "[\\].{0,3}[cC]ite.{0,7}\\{[[:alnum:]]+}" else pat <- "@[[:alnum:]]+"
    tbref <- multiMatch(text = text, pattern = pat)
    ## ----
    if (markdown)
        tbref <- table(sub("@", "", tbref)) else {
        tbref <- sub("[\\].{0,3}[cC]ite.{0,7}\\{", "", tbref)
        tbref <- table(sub("\\}", "", tbref))
    }
    data.frame(key = names(tbref), freq = as.integer(tbref))
}


# NB 2B changed most likely with str_extract()
multiMatch <- function(text, pattern) {
    text <- as.character(text)
    mat <- regexec(pattern = pattern, text)
    ref <- regmatches(text, mat)
    out <- ref[[1L]]
    k <- 1L
    while (k) {
        text <- sub(pattern, "", text)
        mat <- regexec(pattern = pattern, text)
        ref <- regmatches(text, mat)
        if (length(ref[[1L]]) > 0L) {
            out <- c(out, ref[[1L]])
        } else k <- 0L
    }
    out
}