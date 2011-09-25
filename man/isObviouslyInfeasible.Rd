\name{isObviouslyInfeasible}
\alias{isObviouslyInfeasible}
\alias{isObviouslyInfeasible.editarray}
\alias{isObviouslyInfeasible.editmatrix}
\title{Check for obviously infeasible edit rules}
\usage{
  isObviouslyInfeasible(E, ...)

  \method{isObviouslyInfeasible}{editmatrix} (E,
    tol = sqrt(.Machine$double.eps), ...)

  \method{isObviouslyInfeasible}{editarray} (E, ...)
}
\arguments{
  \item{E}{An \code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{...}{Arguments to be passed to or from other
  methods.}

  \item{tol}{Tolerance for checking against zero.}
}
\value{
  a boolean vector of indicating which edits are obviously
  infeasible
}
\description{
  Find any obviously contradictory edit rules.

  Check for obvious infeasible rows, equivalent to 0 < -1.

  Check for edits wich have TRUE in all columns of the
  representation. This corresponds to an espression stating
  that every possible value combination is erroneous.
}
\seealso{
  \code{\link{eliminate}} \code{\link{editmatrix}}
}

