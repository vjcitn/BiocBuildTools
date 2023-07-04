#' define a small collection of packages of interest
#' @param small logical(1) return only 4 packages
#' @return a character vector
#' @examples
#' bioc_coreset()
#' @export
bioc_coreset = function(small=TRUE) {
 if (small) return(c("parody", "vsn", "BiocFileCache", "eds"))
 c("SummarizedExperiment", "GenomicRanges",
   "BiocFileCache", "Rsamtools", "rhdf5", "GEOquery",
   "SingleCellExperiment", "ensembldb", "parody",
   "DESeq2", "EDASeq", "bsseq")
}

setOldClass("biocPkgList")

setClass("PackageSet",
  representation(pkgnames="character", biocversion="character", branchname="character",
    dependencies="list", info="biocPkgList"))

#' constructor for PackageSet instances
#' @importFrom methods new
#' @param cvec character() vector
#' @param biocversion character(1) defaulting to "3.17"
#' @param branchname character(1)
#' @note Will issue message if some element of cvec is not
#' found in BiocPkgTools::biocPkgList() result.  In 2023 function elaborated
#' to check all bioc 'repos': "BioCsoft", "BioCann", "BioCexp",
#'          "BioCworkflows"
#' @examples
#' ps = PackageSet(bioc_coreset())
#' ps
#' ps = add_dependencies(ps)
#' ps
#' @export
PackageSet = function(cvec, biocversion="3.17", branchname="RELEASE_3_17") {
 if (!requireNamespace("BiocPkgTools")) stop("install BiocPkgTools to use this function")
 types = c("BioCsoft", "BioCann", "BioCexp", "BioCworkflows")
 infoset = lapply(types, function(x) BiocPkgTools::biocPkgList(version=biocversion, repo=x)[,c("Package", "Version", "Depends")])
 all_info = do.call(rbind, infoset)
# all_info = BiocPkgTools::biocPkgList(version=biocversion)
 odd = setdiff(cvec, all_info$Package)
 if (length(odd)>0) message("Some elements of cvec are not in Bioconductor.")
 info = all_info[which(all_info$Package %in% cvec),]
 new("PackageSet", pkgnames=cvec, info=info, biocversion=biocversion, branchname=branchname)
}

setMethod("show", "PackageSet",
  function(object) {
  cat("BiocBBSpack PackageSet instance.\n")
  cat(sprintf(" There are %s packages listed (Bioconductor version %s).\n", 
    length(object@pkgnames), object@biocversion))
  cat(sprintf(" There are %s unique dependencies listed.\n", 
    length(unique(unlist(object@dependencies)))))
  cat(sprintf(" Branch name: %s\n", object@branchname))
})

#' a vector listing the key dependencies with correct orthography
#' @return character vector
#' @examples
#' full_dep_opts()
#' @export
full_dep_opts = function() c("Depends", "Imports", "LinkingTo", "Suggests")

.add_dependencies = function(pkgset, deps, omit="R") {
  if (!requireNamespace("BiocPkgTools")) stop("install BiocPkgTools to use this function")
  full_pkg_tbl = BiocPkgTools::buildPkgDependencyDataFrame(
    dependencies = deps, version=pkgset@biocversion)
  kp = full_pkg_tbl[ which(full_pkg_tbl$Package %in% pkgset@pkgnames), ]
  deplist = split(kp$dependency, kp$Package)
  if (length(omit)>0) deplist = lapply(deplist, function(x) setdiff(x, omit))
  pkgset@dependencies = deplist
  pkgset
}

setGeneric("add_dependencies", function(pkgset, deps=full_dep_opts(),
      omit="R")
  standardGeneric("add_dependencies"))
#' method for adding dependencies to PackageSet as determined by BiocPkgTools
#' @aliases add_dependencies
#' @param pkgset PackageSet instance
#' @param deps character() vector of dependency types (e.g., c("Imports", "Depends")); defaults to `full_dep_opts()`
#' @param omit character() vector of 'Depends' options to omit -- for example (default) "R"
#' @export
setMethod("add_dependencies", "PackageSet",
   function(pkgset, deps, omit="R") {
   .add_dependencies(pkgset, deps, omit)
   })
  
#' use git via BiocBBSpack::getpk to retrieve sources into a folder
#' @param pkgset instance of PackageSet
#' @param gitspath character(1) folder to be created if it does not exist
#' @param \dots passed to getpk (might be useful for setting RELEASE_X_XX for git clone)
#' @return invisibly, the list of folders created under gitspath
#' @note Will ignore packageset elements that already have folders in gitspath.
#' @examples
#' ps = PackageSet(bioc_coreset()) # small
#' ps = add_dependencies(ps)
#' tf = tempfile("pop")
#' dir.create(tf)
#' ll = populate_local_gits(pkgset=ps, gitspath=tf)
#' ll
#' @export
populate_local_gits = function(pkgset, gitspath, ...) {
   if (!dir.exists(gitspath)) dir.create(gitspath)
   curd = getwd()
   on.exit(setwd(curd))
   setwd(gitspath)
   curbranch = pkgset@branchname
   ans = lapply(pkgset@pkgnames, function(x) if (!dir.exists(x)) try(getpk(x, branch=curbranch, ...)))
   chk = sapply(ans, inherits, "try-error")
   if (any(chk)) message("there was a try-error thrown; check contents of gitspath")
   invisible(dir(gitspath, full.names=TRUE))
}

read_descriptions = function(gitspath, fields=c("Package", "Version")) {
 stopifnot(dir.exists(gitspath))
 tops = dir(gitspath, full.names=TRUE)
 ds = paste0(tops, "/DESCRIPTION")
 lapply(ds, read.dcf, fields=fields)
}

#' check all repos in a folder for version entry less than
#' the one reported by BiocPkgTools::biocPkgList
#' @param gitspath character(1) folder where repos for packages are cloned
#' @param biocversion character(1) defaults to "3.11"
#' @note DESCRIPTION will be read from each folder in gitspath.
#' @return a character vector of names of packages whose git sources are out of date
#' @examples
#' ps = PackageSet(c("parody", "ensembldb")) # bioc_coreset()[c(3,8)]) # two simple packages
#' tf = tempfile()
#' dir.create(tf)
#' ll = populate_local_gits(ps, tf)
#' curd = getwd()
#' setwd(tf)
#' pd = readLines("parody/DESCRIPTION")
#' pd = gsub("Version.*", "Version: 1.0", pd) # version here < version in git so need to update here
#' writeLines(pd, "parody/DESCRIPTION")
#' local_gits_behind_bioc(tf)
#' setwd(curd)
#' unlink(tf)
#' @export
local_gits_behind_bioc = function(gitspath, biocversion="3.11") {
 if (!requireNamespace("BiocPkgTools")) stop("install BiocPkgTools to use this function")
 ds = read_descriptions(gitspath)
 curinfo = BiocPkgTools::biocPkgList(version=biocversion)
 pks = sapply(ds, "[", 1)
 info = curinfo[which(curinfo$Package %in% pks), c("Package", "Version")]
 basevers = info$Version
 names(basevers) = info$Package
 basevers = basevers[pks] # ensure common ordering
 vs = sapply(ds, "[", 2)
 chk = which(vs < basevers)
 pks[chk]
}

#' not clear that this is needed
#' provide a list of packages for which dependencies are not installed
#' @param gitspath character(1) path to cloned repos
#' @param dependencies character vector of dependency types, probably don't need "Suggests"
local_gits_with_uninstalled_dependencies = function(gitspath,
   dependencies = c("Depends", "Imports", "LinkingTo")) {
 pks = dir(gitspath)
 pset = PackageSet(pks)
 pset = add_dependencies(pset, deps=dependencies) # may not want Suggests
 all_needed = unique(c(pset@pkgnames, unlist(pset@dependencies)))
 allinst = rownames(installed.packages())
 absent = setdiff(all_needed, allinst)
 chk = lapply(pset@dependencies, function(x)
    intersect(x, absent))
 hasabs = sapply(chk, function(x) length(x)>0)
 chk[hasabs]
}

#' not exported, rely instead on rcmdcheck
#' do check_built and clean up
#' @param x path to package tarball
bbs_check_built = function (x) 
{
    if (!requireNamespace("devtools")) stop("install devtools to use this function")
    devtools::check_built(x, args = c("--no-build-vignettes", "--no-install"), 
        check_dir = ".")
    unlink(basename(x))
    pn = gsub("_.*", "", basename(x))
    unlink(qq <- paste0(pn, ".Rcheck/00_pkg_src"), recursive=TRUE, force=TRUE)
    print(qq)
    unlink(paste0(pn, ".Rcheck/", pn), recursive=TRUE, force=TRUE)
}


#' list packages absent from current installation
#' @param pset PackageSet instance
#' @export
absent = function(pset) {
  ii = rownames(installed.packages())
  setdiff(pset@pkgnames, ii)
}
