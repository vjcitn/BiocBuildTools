#' parse an installation log for ERROR
#' @param logfile character(1) path to logfile
#' @param phase character(1) currently defaults to "ERROR", used to filter lines of log
#' @export
explain_absent = function(logfile, phase="ERROR") {
 li = readLines(logfile)
 er = grep(phase, li, value=TRUE)
 ert = gsub(".* package ", "", er)
 ert = gsub("'", "", ert)
 data.frame(pkg=ert,event=er, phase=phase)
}
