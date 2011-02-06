\name{isObviouslyRedundant.matrix}
\alias{isObviouslyRedundant.matrix}
\title{Redundancy check, matrix method...}
\usage{\method{isObviouslyRedundant}{matrix}(E, operators, tol=sqrt(.Machine$double.eps), duplicates=TRUE,
    duplicates.tol=tol, ...)
}
\description{Redundancy check, \code{matrix} method}
\seealso{\code{\link{isObviouslyRedundant}}, \code{\link{isObviouslyRedundant.editmatrix}}}
\arguments{\item{E}{Augmented matrix [A|b] or \code{\link{editmatrix}}}
\item{...}{parameters to be passed to other methods.}
\item{operators}{character vecor of comparison operators in \code{<, <=, ==} of length \code{nrow(E)}}
\item{tol}{tolerance to check for zeros.}
\item{duplicates}{logical: check for duplicate rows?}
\item{duplicates.tol}{tolerance for duplicate search}
}

