\name{localize}
\alias{localize}
\title{Workhorse function for localizeErrors}
\usage{
  localize(E, dat, verbose, pretext, call = sys.call(),
  weight, maxduration, ...)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{dat}{\code{data.frame}}

  \item{verbose}{\code{logical} print progress report
  during run?}

  \item{pretext}{\code{character} text to print before
  progress report}

  \item{weight}{vector or array of weights}

  \item{call}{call to include in
  \code{\link{errorLocation}} object}

  \item{maxduration}{max time for searchBest()}
}
\description{
  Workhorse function for localizeErrors
}
\keyword{internal}

