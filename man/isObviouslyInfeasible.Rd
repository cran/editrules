\name{isObviouslyInfeasible}
\alias{isObviouslyInfeasible}
\title{Check for obvious contradictions in set of (in)equalities...}
\usage{isObviouslyInfeasible(E, tol=sqrt(.Machine$double.eps))
}
\description{Check for obvious contradictions in set of (in)equalities}
\details{If any edit in E is an obvious contradictions of a form similar to 0 < -1, the function.
returns TRUE, otherwise FALSE. Obvious inconsistencies may arise during elimination processes.}
\seealso{\code{\link{eliminateFM}} \code{\link{editmatrix}}}
\arguments{\item{E}{An normalized \code{link{editmatrix}}. If E is not normalized on entry, it will be normalized internally prior to checking.}
\item{tol}{Tolerance for checking against zero.}
}

