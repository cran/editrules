\name{localize_mip_rec}
\alias{localize_mip_rec}
\title{Localize errors by using lpSolveApi}
\usage{
  localize_mip_rec(E, x, weight = rep(1, length(x)),
    maxduration = 600, verbose = "neutral", ...)
}
\arguments{
  \item{E}{editmatrix}

  \item{x}{named numeric with data}

  \item{weight}{numeric with weights}

  \item{maxduration}{number of seconds that is spent on
  finding a solution}
}
\value{
  list with w, adapt and x_c
}
\description{
  \code{localize_mip_rec} uses \code{E} and \code{x} to
  define a mixed integer problem and solves this problem
  using \code{lpSolveApi}. This function can be much faster
  then \code{errorLocalizer} but does not return the
  degeneracy of a solution. However it does return an
  bonus: \code{x_feasible}, a feasible solution
}
\keyword{internal}

