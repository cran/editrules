\name{editsinfo}
\alias{editsinfo}
\title{Retrieve editinfo on an editmatrix...}
\usage{editsinfo(x)
}
\description{Retrieve editinfo on an editmatrix}
\details{If \code{x} is a normal matrix, the matrix will be considered an \code{editmatrix}. The columns of the matrix
are the variables and the rows are the edit rules (contraints).}
\seealso{\code{\link{editmatrix}}}
\value{\code{data.frame} with information on all edit/constraint rules}
\arguments{\item{x}{\code{\link{editmatrix}}  or \code{matrix} object}
}

