\name{isObviouslyRedundant}
\alias{isObviouslyRedundant}
\title{Find obvious redundancies in set of (in)equalities...}
\usage{isObviouslyRedundant(E, ...)
}
\description{Find obvious redundancies in set of (in)equalities}
\details{The function returns a logical vector which is TRUE at any row of the system 
Ax <operators> b which is obviously redundant. Obvious redundancies, amounting
to statements as 0==0 or 0 < 1 may arise durining elimination processes. The 
function also checks for duplicate rows in the augmented matrix [A|b]

Extra paramters:
\itemize{
\item{tol: tolerance to check for zeros, default square root of machine accuracy}
\item{duplicates: logical, check for duplicate rows?, default \code{TRUE}}
\item{duplicates.tol: tolerance for duplicate search, standard: \code{tol}}
}}
\seealso{\code{\link{isObviouslyRedundant.matrix}}, \code{\link{isObviouslyRedundant.editmatrix}}}
\arguments{\item{E}{Augmented matrix A|b or editmatrix}
\item{...}{parameters to be passed to other methods.}
}

