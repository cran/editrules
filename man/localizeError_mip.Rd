\name{localizeError_mip}
\alias{localizeError_mip}
\title{Localize an error using lpSolveApi
localization}
\usage{
  localizeError_mip(E, x, weight = rep(1, length(x)),
  verbose = "neutral", ...)
}
\arguments{
  \item{E}{editmatrix}

  \item{x}{named numeric with data}

  \item{weight}{numeric with weights}
}
\value{
  list with w, adapt and x_c
}
\description{
  Localize an error using lpSolveApi localization
}
\keyword{internal}

