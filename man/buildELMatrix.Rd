\name{buildELMatrix}
\alias{buildELMatrix}
\title{Extends an editmatrix with extra constraints needed for error
localization}
\usage{
  buildELMatrix(E, x, weight = rep(1, length(x)))
}
\arguments{
  \item{E}{editmatrix}

  \item{x}{named numeric with data}
}
\value{
  list with extended E, objfn and lower and upper bound
}
\description{
  Extends an editmatrix with extra constraints needed for
  error localization
}
\keyword{internal}

