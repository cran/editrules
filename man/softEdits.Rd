\name{softEdits}
\alias{softEdits}
\title{Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for
constructing a mip/lp problem.}
\usage{
  softEdits(E, prefix = "adapt.")
}
\arguments{
  \item{E}{normalized \code{editmatrix}}

  \item{xlim}{\code{matrix} with columns lower and upper
  boundary, and rows are variables (in same order as E)}

  \item{prefix}{\code{character} used for naming dummy
  variables in matrix.}
}
\description{
  Derive editmatrix with soft constraints based on
  boundaries of variables. This is a utility function that
  is used for constructing a mip/lp problem.
}
\keyword{internal}

