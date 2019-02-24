#' @title Detect a command binary
#'
#' @description \code{detectBin} detects and returns the path to a command binary.
#'
#' @param bin a system command to be detected, as a character string.
#'
#' @export
#'
#' @return
#' The path to the command binary.
#'
#' @examples
#' detectBin('pandoc')


detectBin <- function(bin) {

    bin <- as.character(bin)

    owarn <- options()$warn
    options(warn = -1)
    on.exit(options(warn = owarn))

    if (grepl("linux|apple", tolower(R.version$os))) {
        out <- system2(command = "which", args = bin, stdout = TRUE, stderr = TRUE)
    } else {
        out <- system(paste0("where ", bin), show.output.on.console = FALSE)
    }

    if (!length(out))
        out <- NA_character_

    out
}
