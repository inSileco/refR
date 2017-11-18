#' @title Convert a BibTeX to a YAML file
#'
#' @description This function converts one (or many) BibTeX into YAML format using the software pandoc and pandoc-citeproc (called with the R function\code{system()}).
#'
#' @param data A vector of collapsed BibTeX [strings] (i.e. each element of the vector/list is one single BibTeX)
#' @param file A vector of BibTeX files to be converted
#' @param path The path to the directory where YAML files will be written (ignored if \code{write = FALSE})
#' @param write A boolean indicating if YAML will be written
#'
#' @export
#'
#' @return A list of YAML files (i.e. a tagged list). If \code{write = TRUE} YAML files are also written in the directory specified by the argument \code{path}.
#'
#' @details User has to specify one of \code{data} or \code{file} otherwise the function will return an error.
#'
#' @seealso \code{\link[refR]{getRefs}}
#'
#' @examples
#' # Coming soon...

bib2yaml <- function(data = NULL, file = NULL, path = ".", write = TRUE) {

    owarn <- options()$warn
    options(warn = -1)
    on.exit(options(warn = owarn))

    ##--- CHECKS FOR PANDOC AND PANDOC-CITEPROC
    pandoc <- detectBin("pandoc")
    if (is.na(pandoc))
        stop("refR requires the installation of pandoc")

    citeproc <- detectBin("pandoc-citeproc")
    if (is.na(citeproc))
        stop("refR requires the installation of pandoc-citeproc.")



    if ((!is.null(data) && !is.null(file)) || (is.null(data) && is.null(file))) {
        stop("Please use data **OR** file.")
    }


    k <- 0

    if (!is.null(data)) {

        if (is.list(data)) {

            if (sum(unlist(lapply(data, function(x) length(x)))) != length(data)) {

                stop("Argument data must be a vector or a single list (not a nested list).")
            }

            data <- unlist(data)
        }

        if (length(data) == 1) {

            data2 <- NULL

        } else {

            data2 <- list()
        }

        for (i in 1:length(data)) {

            if (length(grep("^@[[:alpha:]]{1,}\\{", data[i])) > 0) {

                filename <- strsplit(data[i], "\\{|,")[[1]][2]

                cat(data[i], file = paste0(path, "/_", filename, ".bib"))

                msg <- try(system2(command = "pandoc-citeproc", args = c("--bib2yaml ",
                  paste0(path, "/_", filename, ".bib > ", path, "/", filename, ".yml"))))

                x <- file.remove(paste0(path, "/_", filename, ".bib"))

                if (msg == 0) {

                  if (length(data) == 1) {

                    data2 <- yaml::yaml.load_file(paste0(path, "/", filename, ".yml"))$references[[1]]

                  } else {

                    data2[[k + 1]] <- yaml::yaml.load_file(paste0(path, "/", filename,
                      ".yml"))$references[[1]]
                  }

                  if (!write) {

                    x <- file.remove(paste0(path, "/", filename, ".yml"))
                  }

                  k <- k + 1
                }
            }
        }

        if (k == 0) {

            stop("Argument data does not contain BiBTeX information.")
        }
    }


    if (!is.null(file)) {

        file <- unlist(file)

        if (length(file) == 1) {

            data2 <- NULL

        } else {

            data2 <- list()
        }

        for (i in 1:length(file)) {

            file[i] <- gsub("\\.bib", "", file[i], ignore.case = TRUE)

            if (length(which(dir() == paste0(file[i], ".bib"))) == 1) {

                data <- paste0(readLines(paste0(file[i], ".bib")), collapse = "\n")

                if (length(grep("^@[[:alpha:]]{1,}\\{", data)) == 1) {

                  system(paste0("pandoc-citeproc --bib2yaml ", file[i], ".bib > ",
                    path, "/", file[i], ".yml"))

                  msg <- try(system2(command = "pandoc-citeproc", args = c("--bib2yaml ",
                    paste0(file[i], ".bib > ", path, "/", file[i], ".yml"))))

                  if (msg == 0) {

                    if (length(file) == 1) {

                      data2 <- yaml::yaml.load_file(paste0(path, "/", file[i], ".yml"))$references[[1]]

                    } else {

                      data2[[k + 1]] <- yaml::yaml.load_file(paste0(path, "/", file[i],
                        ".yml"))$references[[1]]
                    }

                    if (!write) {

                      x <- file.remove(paste0(path, "/", file[i], ".yml"))
                    }

                    k <- k + 1
                  }
                }
            }
        }

        if (k == 0) {

            stop("There is no BiBTeX files (or files are corrupted) to convert in your working directory.")
        }
    }

    cat(paste0("\n>>> ", k, " BiBTeX files have successfully been converted in YAML.\n\n"))

    options(warn = 0)
    return(data2)
}
