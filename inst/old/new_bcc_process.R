
get_pkgname = function (x) x$metadata$Package
get_notes = function (x) {
    noo = x$note
    if (length(noo)==0) return("No bioccheck notes")
    unname(sapply(noo, function(x) grep("NOTE:", x, value = TRUE)))
}
get_warnings = function (z) {
  if (length(z$warning)==0) return("No bioccheck warnings")
  sapply(z$warning, function(x) unname(grep("WARNING", x, value = TRUE)))
}
get_errors = function (z) {
  if (length(z$error)==0) return("No bioccheck errors")
  sapply(z$error, function(x) unname(grep("ERROR", x, value = TRUE)))
}

#' now that BiocCheck in 3.15 uses an envRefClass, we must operate on BiocCheck at once
#' @param folder a path to checkout of a package
#' @examples
#' pks = dir(system.file("demo_srcs", package="BiocBuildTools"), full.names=TRUE)
#' dfs = lapply(pks, bcc_to_dataframes)
#' head(dfs[[1]][[1]])
#' @export
bcc_to_dataframes = function(folder) {
  owd = getwd()
  setwd(folder)
  on.exit(setwd(owd))
  x = BiocCheck::BiocCheck()
  bcclist = list(x)
  names(bcclist) = basename(folder)
  pks = sapply(bcclist, get_pkgname)
  names(bcclist) = pks
  basic = data.frame(package=pks)
  bcwarnings = lapply( bcclist, get_warnings)
  bcerrors = lapply( bcclist, get_errors)
  bcnotes = lapply( bcclist, get_notes)
  nwarn = sapply(bcwarnings, length)
  nerr = sapply(bcerrors, length)
  nno = sapply(bcnotes, length)
  bcwarnings = data.frame( package=rep(pks, nwarn), warning = unname(unlist(bcwarnings)))
  bcerrors = data.frame( package=rep(pks, nerr), error = unname(unlist(bcerrors)))
  bcnotes = data.frame( package=rep(pks, nno), note = unname(unlist(bcnotes)))
  list( basic = basic, bcwarnings = bcwarnings, bcerrors=bcerrors, bcnotes=bcnotes)
}
