.onAttach <- function(lib, pkg) {
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- gettextf("%s (version %s %s)", dcf[, "Title"], dcf[, "Version"],
                    dcf[, "Date"])                   
    writeLines(strwrap(msg))
}
