\name{isFeasible}
\alias{isFeasible}
\title{Check consistency of edit set}
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
  Applies Fourier-Motzkin elimination untill either all
  variables are eliminated or the editmatrix becomes
  obviously infeasible. The check rests on the theorem that
  a set of linear inequalities is infeasible if and only if
  0 < -1 can be derived from it.
}

