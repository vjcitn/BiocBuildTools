#' make a serialized collection of pkgnet reports
#' @importFrom pkgnet DependencyReporter FunctionReporter
#' @param src_folders character() collection of source package folders
#' @param target optional character(1) defaults to NULL.  If non-null, a saveRDS will be performed.
#' @return list, each element has components 'dep' and 'fun' corresponding to DependencyReporter
#' and FunctionReporter for each package.  The list is classed as BBT_pkgnet for printing.
#' @examples
#' if (interactive()) {
#'  folds = dir(system.file("demo_srcs", package="BiocBuildTools"), full.names=TRUE)
#'  tf = tempfile()
#'  y = make_pnet_object(folds, target=tf)
#'  x = readRDS(tf)
#'  x$parody$fun$graph_viz
#'  y
#' }
#' @export
make_pnet_object = function(src_folders, target=NULL) {
 bn = basename(src_folders)
 np = length(src_folders)
 pn = vector("list", np)
 for (i in seq_len(np)) {
  pn[[i]] = vector("list", 2)
  names(pn[[i]]) = c("dep", "fun")
  pn[[i]][["dep"]] = DependencyReporter$new()
  pn[[i]][["dep"]]$set_package(bn[i], src_folders[i])
  try({
   pn[[i]][["dep"]]$calculate_default_measures()
  })
  pn[[i]][["fun"]] = FunctionReporter$new()
  pn[[i]][["fun"]]$set_package(bn[i], src_folders[i])
  try({
   pn[[i]][["fun"]]$calculate_default_measures()
  })
  }
 names(pn) = bn
 class(pn) = c("BBT_pkgnet", class(pn))
 if (!is.null(target)) saveRDS(pn, file=target, compress="xz")
 pn
}

#' compact view of a pkgnet list
#' @param x BBT_pkgnet instance
#' @param \dots not used
#' @export
print.BBT_pkgnet = function(x, ...) {
 cat("BBT_pkgnet instance.\n")
 cat(" ", length(x), "elements.\n")
}

