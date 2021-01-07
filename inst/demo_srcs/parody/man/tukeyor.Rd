\name{tukeyor}
\alias{tukeyor}
\alias{rouor}
\alias{hampor}

\title{calibrated outlier region based on various algorithms  }
\description{calibrated outlier region based on various algorithms  }
\usage{
tukeyor(x, alpha=0.05, g=box.scale(length(x), alpha = alpha), ftype="ideal")
}
\arguments{
  \item{x}{ x}
  \item{alpha}{ alpha}
  \item{g}{ g}
  \item{ftype}{ ftype}
}
\author{Vince Carey <stvjc@channing.harvard.edu>}
\examples{
data(tcost)
apply(tcost, 2, tukeyor)
}
\keyword{ models }

