\name{mv.calout.detect}
\alias{mv.calout.detect}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{ calibrated multivariate outlier detection }
\description{
interface to a parametric multivariate outlier detection algorithm
}
\usage{
mv.calout.detect(x, k = min(floor((nrow(x) - 1)/2), 100), Ci = C.unstr, 
    lamfun = lams.unstr, alpha = 0.05, method = c("parametric", 
        "rocke", "kosinski.raw", "kosinski.exch")[1], ...) 

}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ data matrix }
  \item{k}{ upper bound on number of outliers; defaults to just
less than half the sample size }
  \item{Ci}{ function computing Ci, the covariance determinant ratio
excluding row i.  At present, sole
option is \code{C.unstr} (Caroni and
Prescott 1992 Appl Stat).}
  \item{lamfun}{ function computing lambda, the critical values for Ci }
  \item{alpha}{ false outlier labeling rate }
  \item{method}{string identifying algorithm to use}
  \item{\dots}{reserved for future use}
}
\details{
bushfire is a dataset distributed by Kosinski to
illustrate his method.
}
\value{
a list with components
  \item{inds }{indices  of outlying rows}
  \item{vals }{values of outlying rows}
  \item{k }{input parameter k }
  \item{alpha}{input parameter alpha}
}
\references{ C. Caroni and P. Prescott,
Journal of the Royal Statistical Society. Series C (Applied Statistics),
Vol. 41, No. 2 (1992), pp. 355-364 }
\author{ VJ Carey }

\examples{
data(tcost)
mv.calout.detect(tcost)
data(bushfire)
mv.calout.detect(bushfire)
}
\keyword{ models }% at least one, from doc/KEYWORDS


