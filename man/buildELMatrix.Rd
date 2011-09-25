\name{buildELMatrix}
\alias{buildELMatrix}
\title{Extend an editmatrix or editarray with extra constraints needed for error
localization}
\usage{
  buildELMatrix(E, x, weight, ...)
}
\arguments{
  \item{E}{editmatrix}

  \item{x}{named numeric with data}
}
\value{
  list with extended E, objfn and lower and upper bound
}
\description{
  Extend an editmatrix or editarray with extra constraints
  needed for error localization
}
\keyword{internal}

