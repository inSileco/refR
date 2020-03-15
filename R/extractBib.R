#' Substract Bibtex entries.
#'
#' Substarct Bibtex entries that are encountered in a given text.
#'
#' @param bib an object of class `BibEntry` or `bibentry`.
#' @param text a character string to be searched within.
#' @param con a connection object or a character string that stand for a path,
#' ignored if text is defined.
#' @param markdown a logical. If `TRUE`, Markdown formatted citation marks are sought.
#'
#' @export
#' @examples
#' file.name <- system.file('Bib', 'RJC.bib', package='RefManageR')
#' bib <- RefManageR::ReadBib(file.name)
#' # bib[[200]]
#' txt <- 'First \\cite{kim1995estimating}, second \\Citep{fu2006statistical}'
#' out <- extractBib(bib, txt)
#' # RefManageR::WriteBib(out, file=tempfile(fileext = '.bib'))

extractBib <- function(bib, text, con, markdown = FALSE) {
    ##--
    stopifnot(any(class(bib) %in% c("BibEntry", "bibentry")))
    ##--
    if (missing(text)) {
        if (missing(con)) {
          stop("Either 'text' or 'con' must be specified.")
        } else citxt <- findRef(con = con, markdown = markdown)
    } else citxt <- findRef(text = text, markdown = markdown)
    ## find which key in bib file is actually cited
    keystxt <- as.character(citxt$key)
    id <- which(unlist(bib$key) %in% keystxt)
    ## ----
    if (!length(id)) {
        warning("No match found.")
        list(keys = NULL, refs = NULL)
    } else list(keys = keystxt, refs = bib[id])
}
