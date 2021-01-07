#' deal with errors at _build_ stage when attempting to check
#' @importFrom utils capture.output
#' @param x character(1) path to source package folder for a package (not a tarball)
#' @param error character() passed to rcmdcheck::rcmdcheck
#' @param \dots passed to rcmdcheck::rcmdcheck, should not include 'error'
#' @return a list, see note.
#' @note If rcmdcheck succeeds without returning `try-error`, this function returns
#' the result of rcmdcheck.  Otherwise it build a list with elements 'package',
#' 'errors', 'warnings', 'notes', 'install_out', 'description' and 'version',
#' in which the 'errors' component includes data from `capture.output` on
#' rcmdcheck attempt.
#' @export
safe_rcmdcheck = function(x, error="never", ...) {
  pkg = basename(x)
  desc = readLines(paste0(x, "/DESCRIPTION"))
  d = read.dcf(textConnection(desc))
  vers = d[,"Version"]
  basic = capture.output(tmp <- try(rcmdcheck::rcmdcheck(x, error=error, ...)))
  if (!inherits(tmp, "try-error")) return(tmp)
  inst_out = "rcmdcheck threw error"
  ans = list(package=pkg, errors=basic, warnings="chk threw error", notes="rcmdcheck threw error",
     install_out=inst_out, description=desc, version=vers)
  ans
}

