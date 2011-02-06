\name{isFeasible}
\alias{isFeasible}
\title{Check consistency of editmatrix...}
\usage{isFeasible(E, warn=TRUE)
}
\description{Check consistency of editmatrix}
\details{Applies fourier-motzkin elimination untill either all
variables are eliminated or the editmatrix becomes obviously
infeasible. The check rests on the theorem that a set of linear
inequalities is infeasible if and only if  0 < -1 can be derived from it.}
\value{TRUE or FALSE}
\arguments{\item{E}{an \code{\link{editmatrix}}}
\item{warn}{logical: should a warning be raised when system is infeasible?}
}

