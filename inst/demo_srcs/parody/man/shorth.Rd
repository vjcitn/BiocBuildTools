\name{shorth}
\alias{shorth}
\title{
one-dimensional MVE (min. vol. ellipsoid)
}
\description{
generalized length of shortest-half sample
}
\usage{
shorth(x, Alpha=0.5)
}
\arguments{
\item{x}{
data vector, no NAs
}
\item{Alpha}{
minimum fraction of data to be covered by scale estimator.
if Alpha == 0.5, the \code{shorth} is calculated
}
}
\value{
a list, say L, with components
\item{shorth}{
a 2-vector with endpoints of the shortest Alpha-sample
}
\item{length.shorth}{
see previous return component
\code{L$shorth[2]-L$shorth[1]}
}
\item{midpt.shorth}{
mean(L[["shorth"]])
}
\item{meanshorth}{
mean of values in the shorth, studied by Andrews et al (1972)
as a location estimator
}
\item{correction.parity.dep}{
correction factor to be applied to achieve
approximate unbiasedness and diminish small-sample
parity dependence; \code{L["shorth"]] * L[["correction"]]} is approximately
unbiased for the Gaussian standard deviation, for
0 < Alpha < 1.
}
\item{bias.correction.gau.5}{
correction factor to be applied along with
correction.parity.dep when Alpha = .5; empirically
derived bias correction useful for 10 < N < 2000 and
possibly beyond.  To use, divide: 
\code{(L[["shorth"]] * L[["correction"]] / L[["bias.corr"]])} is approximately
unbiased for Gaussian standard deviation, when Alpha=.5.
}
\item{Alpha}{
coverage fraction used
}}
\references{
Rousseeuw and Leroy, Stat Neer (1988), Gruebel, Ann Stat (1988)
}
\keyword{robust}
% Converted by Sd2Rd version 1.21.



