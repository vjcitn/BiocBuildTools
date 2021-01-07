\name{bushfire}
\alias{bushfire}
\docType{data}
\title{ satellite data on bushfire scars }
\description{
satellite data on bushfire scars
}
\usage{data(bushfire)}
\format{
  The format is:
 num [1:38, 1:5] 111 113 113 110 101 93 92 94 94 100 ...
}
\details{
Satellite measurements on five frequency bands corresponding to
each of 38 pixels.
}
\source{
Maronna RA and Yohai VJ (1995) JASA 90:330.
}
\examples{
data(bushfire)
mv.calout.detect(bushfire)
}
\keyword{datasets}
