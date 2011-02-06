\name{isObviouslyRedundant}
\alias{isObviouslyRedundant}
\title{Find obvious redundancies in set of (in)equalities...}
\usage{isObviouslyRedundant(E, ...)
}
\description{Find obvious redundancies in set of (in)equalities}
\details{The function returns a logical vector which is TRUE at any row of the system 
Ax <operators> b which is obviously redundant. Obvious redundancies, amounting
to statements as 0==0 or 0 < 1 may arise durining elimination processes.}
\seealso{\code{\link{isObviouslyRedundant.matrix}}, \code{\link{isObviouslyRedundant.editmatrix}}}
\arguments{\item{E}{Augmented matrix A|b or editmatrix}
\item{...}{parameters to be passed to other methods.}
}

