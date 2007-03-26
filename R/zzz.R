.onAttach <- function(lib, pkg) {
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- gettextf("%s (%s %s)", dcf[, "Title"], dcf[, "Version"],
                    dcf[, "Date"])
    message(paste(strwrap(msg), collapse = "\n"))
}
