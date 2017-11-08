#' @title Retrieve author references from Scopus and Crossref APIs
#'
#' @description
#' This function queries the Scopus Search API to returned a list of references for one specific author.
#' Quality of metadata is increased by coupling Scopus results with a query of
#' the Crossref API called using the package \code{\link[rcrossref]{rcrossref}}.
#' Results are returned as BibTeX files (one per reference) but user can also choose two other formats: YAML and JSON.
#' The conversion is proceeded using pandoc and pandoc-citeproc (must be separately installed).
#'
#' @param api_key A string indicating the user Scopus API Key obtained from the Elsevier Developer Portal
#' @param author_id A string indicating the Scopus identifier of the author for which references have to be retrieve
#' @param date A string indicating the year (or a range of years) for which references have to be retrieve (if \code{NULL}, default, all references listed in Scopus for this author will be returned)
#' @param sort A string indicating the field by which references will be ordered (one of \code{artnum}, \code{citedby-count}, \code{publicationName}, \code{pubyear})
#' @param sleep A positive numeric indicating the time interval (in seconds) between two consecutive Scopus queries (a Scopus query response is limited to 25 entries)
#' @param folder A string indicating the folder (relative or absolute path) to write references
#' @param format A string indicating the references format (the possible formats are \code{bibtex}, \code{yaml} and \code{json})
#' @param erase A boolean specifing if a reference that already exists in the folder must be rewritten (\code{TRUE}) or not (\code{FALSE})
#'
#' @export
#'
#' @return A list of BibTeX strings. BibTeX (if required) and other specified formats are also written in the directory specified by the argument \code{folder}.
#'
#' @details Before using this function user has to get a free Scopus API Key from the Elsevier Developer Portal (\url{https://dev.elsevier.com/user/registration}).
#' He also needs to know the author Scopus identifer by visiting the Scopus Author Search Portal (\url{https://www.scopus.com/freelookup/form/author.uri}).
#' This function works in four steps:
#' 1) Get a list of references from Scopus API and keep only those with a DOI (all fields are returned except the authors list);
#' 2) Complete the metadata (mainly the authors list) using the Crossref API called by the function \code{\link[rcrossref]{cr_cn}} (request by DOI);
#' 3) Merge metadata from the two sources and keep the best (hopefully);
#' 4) Write references (one file per reference) in the specified formats using pandoc-citeproc (for YAML and JSON).
#' We strongly recommend to convert references in YAML (and also keep BibTeX formats)
#' because this step (performed with pandoc-citeproc) translates a large amount of LaTeX tags (mainly accented characters).
#' But if you prefer you can also export references in BibTeX and then use the function \code{\link[refR]{bib2yaml}} to convert them in YAML.
#'
#' @seealso \code{\link[refR]{bib2yaml}}, \code{\link[refR]{cleanRefs}}
#'
#' @examples
#' # Coming soon...

getRefs <- function(
  api_key   = NULL,
  author_id = NULL,
  date      = NULL,
  sort      = "pubyear",
  sleep     = 5,
  folder    = ".",
  format    = c("bibtex", "yaml"),
  erase     = FALSE
) {


  options(warn = -1)


  ### CHECKS FOR PANDOC AND PANDOC-CITEPROC

  if (length(grep("linux|apple", tolower(sessionInfo()$platform))) == 1) {

    pandoc   <- system2(command = "which", args = "pandoc", stdout = TRUE, stderr = TRUE)
    citeproc <- system2(command = "which", args = "pandoc-citeproc", stdout = TRUE, stderr = TRUE)

    if (length(pandoc) == 0 || length(citeproc) == 0) {
      stop("The package refR requires the installation of pandoc AND pandoc-citeproc.")
    }

  } else {

    pandoc   <- system("where pandoc", show.output.on.console = FALSE)
    citeproc <- system("where pandoc-citeproc", show.output.on.console = FALSE)

    if (pandoc == 1 || citeproc == 1) {
      stop("The package refR requires the installation of pandoc AND pandoc-citeproc.")
    }
  }


  ### CHECKS FOR FUNCTION ARGUMENTS

  if (is.null(api_key)) {
    stop("Please provide a Scopus API Key.\nVisit this website \"https://dev.elsevier.com\" for further informations.")
  }

  if (is.null("author_id")) {
    stop("Please provide an Author Scopus ID.\nVisit this website \"https://www.scopus.com/freelookup/form/author.uri\" to get one.")
  }

  if (!is.null(date)) {
    if (length(date) > 1) {
      stop("The argument \"date\" must be a year (YYYY) or a range of years (YYYY-YYYY).")
    } else {
      if (!length(grep("^[0-9]{4}$|^[0-9]{4}[[:punct:]][0-9]{4}$", date))) {
        stop("The argument \"date\" must be a year (YYYY) or a range of years (YYYY-YYYY).")
      }
    }
    date <- gsub("[[:punct:]]", "-", date)
  }

  if (!is.null(sort)) {
    sort <- tolower(sort)
    if (!(sort %in% c("artnum", "citedby-count", "publicationName", "pubyear"))) {
      stop("Available sort options: \"artnum\" or \"citedby-count\" or \"publicationName\" or \"pubyear\"")
    }
  }

  if (!(folder %in% c(".", "./"))) {
    if (length(which(dir() == gsub("/", "", folder))) == 0) {
      stop(paste0("The directory '", folder, "' does not exist. Please select an appropriate folder."))
    }
  }

  format <- tolower(format)
  format <- gsub("yml", "yaml", format)
  if (!(format %in% c("bibtex", "yaml", "json"))) {
    stop("Supported formats: \"bibtex\" or \"yaml\" or \"json\"")
  }


  cat("\n------------------------------------------------\n")
  cat(paste0("\n       >>>   Scopus - Crossref APIs   <<<       \n"))
  cat("\n------------------------------------------------\n")
  cat("\n")
  cat(paste0("\r   [] Author ID    - ", author_id, "\n"))
  cat("\n")


  ### SCOPUS API PARAMETERS

  tags <- c(
    "dc:identifier",
    "subtypeDescription",
    "prism:coverDate",
    "dc:creator",
    "dc:title",
    "prism:publicationName",
    "prism:volume",
    "prism:issueIdentifier",
    "prism:pageRange",
    "prism:isbn",
    "prism:doi",
    "citedby-count"
  )

  tags_names <- c(
    "scopus_id",
    "doctype",
    "date",
    "author",
    "title",
    "journal",
    "volume",
    "issue",
    "pages",
    "isbn",
    "doi",
    "citedby"
  )


  w        <-  1
  from     <-  0
  nmax     <- 25
  is_entry <- TRUE
  items    <- list()

  while (is_entry) {

    ### Scopus request

    res <- fromJSON(
      file = paste0(
        "https://api.elsevier.com/content/search/scopus?apiKey=", api_key,
        "&query=AU-ID(", author_id, ")",
        "&start=", from,
        "&count=", nmax,
        ifelse(!is.null(date), paste0("&date=", date), ""),
        ifelse(!is.null(sort), paste0("&sort=", sort), ""),
        "&httpAccept=application/json"
      )
    )

    ### Is there reference(s)?

    if (length(which(names(res[[1]]) == "entry")) > 0) {


      ### If Result set was empty

      if (length(which(names(res[[1]]$entry[[1]]) == "error")) > 0) {


        ### Exit

        is_entry <- FALSE

      } else {

        cat(paste0("\r   [] Scopus API   - ", (length(items) + length(res[[1]]$entry)), " items found"))


        ### Extract metadata

        for (k in 1:length(res[[1]]$entry)) {

          infos <- list()

          for (j in 1:length(tags)) {

            info <- res[[1]]$entry[[k]][[tags[j]]]

            if (tags[j] == "prism:coverDate") {
              info <- substr(as.character(info), 1, 4)
            }
            infos[[tags_names[j]]] <- ifelse(!is.null(info), info, "")
          }

          infos[["scopus_id"]] <- gsub("SCOPUS_ID:", "", infos[["scopus_id"]])

          items[[w]] <- infos
          w <- w + 1
        }

        ### By-pass Scopus blocking system (in seconds)

        Sys.sleep(sleep)


        ### Incrementation of start entry (next loop)

        from <- from + nmax
      }

    } else {


      ### Exit

      is_entry <- FALSE
    }
  }


  ### If at least one result returned

  if (length(items) > 0) {


    ### Remove items w/o DOI

    dois <- unlist(lapply(items, function(x) x$doi))
    pos  <- which(dois == "")

    cat(paste0("\n                   - ", (length(items) - length(pos)), " items with a DOI\n"))

    if (length(pos) > 0) {
      items <- items[-pos]
    }


    ### Do not re-download citation if already exist

    if (!erase) {

      ids   <- paste0("ID", unlist(lapply(items, function(x) x$scopus_id)))
      files <- unique(gsub("\\.bib|\\.yml|\\.json", "", dir(folder)))

      pos <- which(ids %in% files)
      if (length(pos) > 0) {
        items <- items[-pos]
      }

      cat(paste0("                   - ", length(items), " new items\n\n"))
    } else {
      cat("\n")
    }

    ### If at least one DOI

    if (length(items) > 0) {


      w     <- 1
      refs  <- NULL

      for (i in 1:length(items)) {


        ### Get citation from CROSSREF

        Sys.sleep(.5)
        ref <- cr_cn(doi = items[[i]]$doi, format = "bibtex")


        ### If the reference has been found

        if (!is.null(ref)) {


          cat(paste0("\r   [] Crossref API - ", w, " citations found"))


          ### Convert string to vector

          ref <- strsplit(ref, "\n|\n\t")[[1]]


          ### If it is an inline BiBTeX

          if (length(ref) == 1) {
            ref <- strsplit(ref, ",")[[1]]
            ref <- gsub("^ ", "", ref)
          }


          ### If one tag is on two lines (bug for one ref on 3000 tested)

          pos <- which(!grepl(",$|}$", ref))

          if (length(pos) > 0) {

            if (length(grep("[[:alpha:]]{1,} = ", ref[pos + 1])) == 0) {

              ref[pos + 1] <- gsub("\\t", "", ref[pos + 1])
              ref[pos] <- paste0(ref[pos], ref[pos + 1], collapse = "")
              ref <- ref[-(pos + 1)]
            }
          }


          ### Remove blank row

          pos <- which(ref == "")

          if (length(pos) > 0) {

            ref <- ref[-pos]
          }


          ### Remove final }

          pos <- grep("^\\}$", ref)

          if (length(pos) > 0) {

              ref <- ref[-pos]

          } else {

            ref <- gsub("\\}\\}$", "}", ref)
          }


          ### Change BiBTeX Key (by Article Scopus ID)

          pos_key <- grep("^@", ref)
          key <- strsplit(ref[pos_key], "\\{|,")[[1]]
          ref[pos_key] <- paste0(key[1], "{ID", as.character(items[[i]]$scopus_id), ",")


          ### Clean TITLE

          if (items[[i]]$title != "") {
            pos <- grep("^title", ref)
            if (length(pos) > 0) {
              ref[pos] <- paste0("title = {", items[[i]]$title, "},")
            } else {
              ref <- c(ref, paste0("title = {", items[[i]]$title, "},"))
            }
          }

          ref <- gsub("\\( ", "(", ref)
          ref <- gsub(" \\)", ")", ref)


          ### Clean AUTHOR firstname

          ref[grep("^author", ref)] <- gsub("\\.", "\\. ", ref[grep("^author", ref)])
          ref[grep("^author", ref)] <- gsub(" -|- | - ", "-", ref[grep("^author", ref)])
          ref[grep("^author", ref)] <- gsub("[[:space:]]+", " ", ref[grep("^author", ref)])


          ### Clean hyphen in pages range

          ref[grep("^pages", ref)] <- gsub("-{1,}", "-", ref[grep("^pages", ref)])


          ### Add other tags (if missing from CROSSREF)

          tags_scopus <- c("doi", "date", "volume", "issue",  "pages", "journal")
          tags_bibtex <- c("doi", "year", "volume", "number", "pages", "journal")

          for (j in 1:length(tags_scopus)) {

            if (items[[i]][[tags_scopus[j]]] != "") {

              pos <- grep(paste0("^", tags_bibtex[j]), ref)

              if (length(pos) == 0) {

                ref <- c(ref, paste0(tags_bibtex[j], " = {", items[[i]][[tags_scopus[j]]], "},"))
              }
            }
          }

          ref <- paste0(ref, ",")
          ref <- gsub(",,", ",", ref)


          ### Correct a f****** bug

          ref <- gsub("\\{\\\\~\\{A\\}\\}\u008e", "I", ref)


          ### Re-build the BiBTeX

          ref[length(ref)] <- gsub("\\},$", "\\}", ref[length(ref)])
          ref <- paste0(paste0(ref, collapse = "\n\t"), "\n}")

          refs[w] <- ref
          w <- w + 1
        }
      }


      cat("\n")
      cat(paste0("\n   [] Exporting citations in ", paste0(toupper(format), collapse = ", ")))
      cat("\n")


      for (i in 1:length(refs)) {


        ### Get filename (Article Scopus ID)

        filename <- strsplit(refs[[i]], "\\{|,")[[1]][2]


        ### Export BiBTeX

        cat(refs[[i]], file = paste0(folder, "/", filename, ".bib"))


        ### Conversion(s) with PANDOC

        for (j in 1:length(format)) {

          if (format[j] == "yaml") {

            system(paste0("pandoc-citeproc --bib2yaml ", folder, "/", filename, ".bib > ", folder, "/", filename, ".yml"))
          }

          if (format[j] == "json") {

            system(paste0("pandoc-citeproc --bib2json ", folder, "/", filename, ".bib > ", folder, "/", filename, ".json"))
          }
        }


        ### Remove temporary BiBTeX (if required)

        if (!("bibtex" %in% format)) {

          xxx <- file.remove(paste0(folder, "/", filename, ".bib"))
        }
      }


      cat(paste0("\nFiles have been successfully written in ", ifelse(folder == ".", getwd(), folder)))
      cat("\n")
      cat("\n------------------------------------------------\n\n")


    } else {


      cat(paste0("   [] No new citations found"))
      cat("\n")
      cat(paste0("      Exit..."))
      cat("\n")
      cat("\n------------------------------------------------\n\n")
    }


  } else {


    cat(paste0("   [] Scopus API   - No items found"))
    cat("\n")
    cat(paste0("      Exit..."))
    cat("\n")
    cat("\n------------------------------------------------\n\n")
  }

  options(warn = 0)

  return(refs)
}
