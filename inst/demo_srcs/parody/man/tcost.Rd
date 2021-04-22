\name{tcost}
\alias{tcost}
\docType{data}
\title{ Data on milk transportation costs, from Johnson and Wichern,
Applied Multivariate Statistical Analysis, 3rd edition }
\description{
Multivariate data on milk transportation costs
}
\usage{data(tcost)}
\format{
  The format is:
 num [1:36, 1:3] 16.44 7.19 9.92 4.24 11.2 ...
 
 - attr(*, "dimnames")=List of 2
  \describe{
    \item{\code{..$}:}{chr [1:36] "1" "2" "3" "4" ...}

    \item{\code{..$}:}{chr [1:3] "fuel" "repair" "capital"}
  }
}
\details{
Extract from Johnson and Wichern example dataset on milk
transportation.
}
\source{
Johnson and Wichern,
Applied Multivariate Statistical Analysis, 3rd edition, p263 }

%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(tcost)
mv.calout.detect(tcost)
}
\keyword{datasets}
