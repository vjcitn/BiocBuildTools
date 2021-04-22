\name{calout.detect}
\alias{calout.detect}
\title{
interface to modular calibrated outlier detection system
}
\description{
Various classical and resistant outlier detection procedures
are provided in which the outlier misclassification rate
for Gaussian samples is fixed over a range of sample sizes.
}
\usage{
calout.detect(x, alpha = 0.05, method = c("GESD", "boxplot", "medmad", 
    "shorth", "hybrid"), k = ((length(x)\%\%2) * floor(length(x)/2) + 
    (1 - (length(x)\%\%2)) * (length(x)/2 - 1)), scaling, ftype, 
    location, scale, gen.region = function(x, location, scale, 
        scaling, alpha) {
        g <- scaling(length(x), alpha)
        location(x) + c(-1, 1) * g * scale(x)
    })
}
\arguments{
\item{x}{
data vector, NAs not allowed
}
\item{alpha}{
outlier mislabeling rate for Gaussian samples
}
\item{method}{
one of c("GESD", "boxplot", "medmad", "shorth"); the first
selects generalized extreme studentized deviate (Rosner, 1983);
the second selects calibrated boxplot rules; the third selects
the method of Hampel in which the sample median is used for
location estimation, and the median absolute deviation is used
for scale; and the fourth selects Rousseeuw's rule, with
the midpoint of the shortest half sample used as location
estimator, and the length of this shortest half sample used 
as scale estimator.

An important characteristic of the GESD procedure is that
the critical values for outlier labeling are calibrated to
preserve the overall Type I error rate of the procedure
given that there will be k tests, whether or not any
outliers are present in the data.
}
\item{k}{
for GESD, the prespecified upper limit on the number of 
outliers suspected in the data; defaults to ``half'' the sample size.
}
\item{scaling}{
for resistant methods, scaling is a sample-size
dependent function that tells how many multiples of the scale
estimate should be laid off on each side of the
location estimate to demarcate the inlier region; see Davies and Gather
(1993) for the general formulation.  The main contribution
of this program consists in the development of scaling functions
that ``calibrate'' outlier detection in Gaussian samples.
The scaling function is assumed to take two arguments, n and alpha,
and it should return a real number.


If method=="boxplot", the default value scaling=box.scale will confine
the probability of
erroneous detection of one or more outliers in a pure
Gaussian sample to alpha.  The use of scaling=function(n,alpha) 1.5
gives the standard boxplot outlier labeling rule.


If method=="medmad", the use of scaling=hamp.scale.4 will
confine the outlier mislabeling rate to alpha; whereas
the use of scaling=function(n,alpha) 5.2 gives Hampel's rule
(Davies and Gather, 1993, p. 790).


If method=="shorth", the default value scaling=shorth.scale will
confine the outlier mislabeling rate to alpha.
}
\item{ftype}{
The type of ``fourth'' calculation; the standard definition
of the fourth uses 0.5 * floor((n + 3)/2) to obtain the
sortile of the fourth value; Hoaglin and Iglewicz (1987)
give an ``ideal'' definition of the fourth which reduces
the dependence of boxplot-based outlier detection performance
(in small samples) on the quantity n mod 4.
  }
\item{location}{a function on a vector returning a location estimate}
\item{scale}{a function on a vector returning a scale estimate}
\item{gen.region}{a function of x, location, scale, scaling, alpha
 that returns the inlier region as a 2-vector}
}
\value{
a list with components ind (indices of outliers in the
input vector) val (values of these components) and outlier.region,
which is only defined for the resistant methods. 
}
\references{
Davies and Gather (1993 JASA), Rousseeuw and Leroy (1988 Stat Neer),
Rosner (1983 Technom), Hoaglin and Iglewicz (1987 JASA),
Carey, Walters, Wager and Rosner (1997 Technom)
}
\examples{
lead <- c(83, 70, 62, 55, 56, 57, 57, 58, 59, 50, 51, 52, 52, 52, 54, 54, 45, 46, 48, 
        48, 49, 40, 40, 41, 42, 42, 44, 44, 35, 37, 38, 38, 34, 13, 14)

calout.detect(lead,alpha=.05,method="boxplot",ftype="ideal")
calout.detect(lead,alpha=.05,method="GESD",k=5)
calout.detect(lead,alpha=.05,method="medmad",scaling=hamp.scale.3)
calout.detect(lead,alpha=.05,method="shorth")

}
\keyword{models}
% Converted by Sd2Rd version 1.21.

