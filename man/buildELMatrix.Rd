\name{buildELMatrix}
\alias{buildELMatrix}
\title{Extend an editset with extra constraints needed for error
localization}
\usage{
  buildELMatrix(E, x, weight = rep(1, length(x)),
    xlim = generateXlims(x), maxvalue = 1e+08, ...)
}
\arguments{
  \item{E}{editset}

  \item{x}{named list with data}

  \item{weight}{vector with weights of the variable in the
  same order as x}

  \item{xlim}{upper and lower boundaries of \code{x}}
}
\value{
  list with extended E, objfn and lower and upper bound
}
\description{
  Extend an editset with extra constraints needed for error
  localization
}
\keyword{internal}

