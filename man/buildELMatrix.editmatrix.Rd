\name{buildELMatrix.editmatrix}
\alias{buildELMatrix.editmatrix}
\title{Extend an editmatrix with extra constraints needed for error
localization}
\usage{
  \method{buildELMatrix}{editmatrix} (E, x,
    weight = rep(1, length(x)),
    xlim = 1000 * cbind(l = -abs(x), u = abs(x)),
    maxvalue = 1e+08)
}
\arguments{
  \item{E}{editmatrix}

  \item{x}{named numeric with data}

  \item{weight}{vector with weights of the variable in the
  same order as x}

  \item{xlim}{upper and lower boundaries of \code{x}}
}
\value{
  list with extended E, objfn and lower and upper bound
}
\description{
  Extend an editmatrix with extra constraints needed for
  error localization
}
\keyword{internal}

