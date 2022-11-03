#' use pkgnet to analyze a source folder (including covr statistics)
#' @param sourcedir character(1) should contain an R package
#' @param serialize logical(1) defaults to TRUE, See note.
#' @param overwrite logical(1) defaults to TRUE; if FALSE will fail if `[basename(sourcedir)]_pnet.rds` exists
#' @note By default, serializes an S3 instance of `BBT_pkgnet`, a list with components `dep` for dependency report,
#' and `fun` for function interrelationships, saved as RDS to `[basename(sourcedir)]_pnet.rds`.  If
#' `serialize` is FALSE, simply returns the object.  This is intended to be used
#' on a large number of packages and thus a checkpointing approach is the default.
#' @return See Note.
get_pnet = function(sourcedir, serialize=TRUE, overwrite=TRUE) {  # to run covr
        require(pkgnet)
        bn = basename(sourcedir)
        target = paste0(basename(sourcedir), "_pnet.rds")
        if (!overwrite & file.exists(target)) stop(paste("will not overwrite", target))
        pn = vector("list", 2)
        names(pn) = c("dep", "fun")
        pn[["dep"]] = DependencyReporter$new()
        pn[["dep"]]$set_package(bn, sourcedir)
        try({
            pn[["dep"]]$calculate_default_measures()
        })
        pn[["fun"]] = FunctionReporter$new()
        pn[["fun"]]$set_package(bn, sourcedir)
        try({
            pn[["fun"]]$calculate_default_measures()
        })
        class(pn) = c("BBT_pkgnet", class(pn))
        if (serialize) saveRDS(pn, file = target, compress = "xz")
        else return(pn)
}



#' check an R package tarball
#' @importFrom utils untar capture.output head
#' @param x character(1) path to tarball
#' @param error passed to `rcmdcheck::rcmdcheck`
#' @param args passed to `rcmdcheck::rcmdcheck`
#' @param convert function that converts path to a package name
#' @param \dots passed to `rcmdcheck::rcmdcheck`
#' @note writes objects `[pname]_chk.rds` and `[pname]_bcchk.rds` to disk
#' for later use
#' td = tempdir()
#' cur = getwd()
#' setwd(td)
#' pdy = system.file("demo_srcs/parody", package="BiocBuildTools")
#' pkgbuild::build(pdy)
#' tbcheck(dir(pattern="parody_"))
#' file.exists("parody_chk.rds")
#' setwd(cur)
#' @export
tbcheck = function (x, error = "never", args="--no-build-vignettes",
    convert=basename, ...)
{
    basic = capture.output(tmp <- try(rcmdcheck::rcmdcheck(x,
        error = error, args=args, ...)))
    pname = gsub("_.*", "", x)
    pname = convert(pname)
    if (!inherits(tmp, "try-error")) {
        saveRDS(tmp, file=paste0(pname, "_chk.rds"))
        bc = BiocCheck::BiocCheck(x)
        saveRDS(bc, file=paste0(pname, "_bcchk.rds"))
        return(tmp)
        }
    inst_out = "rcmdcheck threw error"
    td = tempdir()
    dfile = untar(x, file=paste0(pname, "/DESCRIPTION"), exdir=td)
    desc = readLines(dpa <- paste0(td, "/", pname, "/DESCRIPTION"))
    vers = read.dcf(dpa)[,"Version"]
    ans = list(package = x, errors = basic, warnings = "chk threw error",
        notes = "rcmdcheck threw error", install_out = inst_out,
        description = desc, version = vers)
    saveRDS(ans, file=paste0(pname, "_chk.rds"))
    bb = BiocCheck::BiocCheck(x)
    saveRDS(nn, file=paste0(pname, "_bcchk.rds"))
}
