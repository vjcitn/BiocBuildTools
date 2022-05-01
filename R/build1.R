prep1 = function(pkgname) {
        if (!requireNamespace("BiocPkgTools")) stop("install BiocPkgTools to use this function")
        dtypes = c("Depends", "Imports", "Suggests", "LinkingTo")
	ddf = as.data.frame(
           BiocPkgTools::buildPkgDependencyDataFrame(dependencies=dtypes))
	stopifnot(pkgname %in% ddf$Package)
	sdd = split(ddf, ddf$Package)
	targ = sdd[[pkgname]] # little data.frame
	deps = setdiff(targ$dependency, "R")
	ii = rownames(installed.packages())
	to_install = setdiff(deps, ii)
	if (length(to_install)>0) {
	       	ans = try(BiocManager::install(to_install, ask=FALSE, update=FALSE))
		if (inherits(ans, "try-error")) return(ans)
		}
	TRUE
}

#' prepare and build a package tarball, not exported because we use build1_with_buildsink to capture log
#' @importFrom methods is
#' @importFrom utils installed.packages
#' @importFrom pkgbuild build
#' @param srcpath character(1) path to source folder for a package
#' @param dest character(1) destination folder
#' @param \dots pass to pkgbuild::build
#' @note If preparation for building triggers a try-error, the resulting 
#' exception object is returned.  Otherwise the result of pkgbuild::build()
#' is returned.
build1 = function(srcpath, dest=".", ...) {
	n1 = try(prep1(basename(srcpath)))
        if (inherits(n1, "try-error")) {
          message(paste(srcpath))
          message("cannot be prepared")
          }
        if (!is(n1, "logical")) return(n1)
	if (n1) try(pkgbuild::build(srcpath, dest, clean_doc=TRUE, ...)) # avoid query
}

#' run build1 and capture the R CMD build log
#' @param srcpath character(1) path to source folder for a package
#' @param dest character(1) destination folder
#' @param \dots pass to pkgbuild::build
#' @export
build1_with_buildsink = function(srcpath, dest=".", ...) {
	sink(paste(basename(srcpath), ".bldlog.txt", sep=""))
	build1(srcpath=srcpath, dest=dest, ...)
	sink(NULL)
	}

