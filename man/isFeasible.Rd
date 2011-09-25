\name{isFeasible}
\alias{isFeasible}
\title{Check consistency of editmatrix or editarray}
\usage{
  isFeasible(E, warn = FALSE)
}
\arguments{
  \item{E}{an \code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{warn}{logical: should a warning be raised when
  system is infeasible?}
}
\value{
  TRUE or FALSE
}
\description{
  For linear edits, Fourier-Motzkin elimination is applied
  untill either all variables are eliminated or the
  editmatrix becomes obviously infeasible. The check rests
  on the property (Farka's lemma) that a set of linear
  inequalities is infeasible if and only if 0 < -1 can be
  derived from it.
}
\details{
  For categorical edits in an editarray, variables are
  eliminated by category resolution untill all variables
  are eliminated or an obvious infeasibility is
  encountered.
}

