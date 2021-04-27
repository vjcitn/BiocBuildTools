#' use BiocManager::install to get any unavailable dependencies as defined by BiocPkgTools
#' @param pkg character(1) name of package
#' @param depdf a data.frame like the output of `BiocPkgTools::buildPkgDependencyDataFrame()`
#' @param exclude character() of uninstalled packages to be ignored -- BiocManager::install is slow
#' to deal with an unavailable package
#' @param \dots passed to BiocManager::install
#' @return NULL
#' @note Function is used only for side-effect of installation.  `exclude` defaults to
#' 'KEGG.db' because in Bioc 3.13 that is not available.  It is understood that code
#' that performs installation is poor style for scientific programming; this package
#' concerns software ecosystem management and must be able to programmatically
#' define the ecosystem for evaluation.
#' @export
fulfill_deps = function(pkg, depdf = NULL, exclude="KEGG.db", ...) {
 oopt = options(no.readonly=TRUE)
 on.exit(options(oopt))
 options(timeout=300)
 ii = rownames(installed.packages())
 if (is.null(depdf)) depdf = BiocPkgTools::buildPkgDependencyDataFrame()
 dat = dplyr::filter(depdf, Package == pkg)
 must_get = setdiff(dat$dependency, c("R", ii, exclude))
 if (length(must_get)>0) message(paste("running BiocManager::install on", must_get))
 if (length(must_get)>0) BiocManager::install(must_get, ...)
 return(NULL)
}
