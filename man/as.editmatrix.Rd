\name{as.editmatrix}
\alias{as.editmatrix}
\title{Coerce to an edit matrix.}
\usage{as.editmatrix(x, C=numeric(nrow(mat)), ops=rep("==", nrow(mat)), ...)
}
\description{Coerce to an edit matrix. This method will derive editrules from a matrix.}
\details{\code{as.editmatrix} interpretes the matrix as an editmatrix and derives readable edit rules. 
The columns of the matrix
are the variables and the rows are the edit rules (contraints).

If only argument \code{x} is given (the default), the resulting editmatrix is of the form \eqn{Ex=0}. 
This can be influenced by using the parameters \code{C} and \code{ops}.}
\seealso{\code{\link{editmatrix}}}
\value{an object of class \code{editmatrix}.}
\arguments{\item{x}{object to be transformed into an \code{\link{editmatrix}}. \code{x} will be coerced to a matrix.}
\item{C}{Constant, a \code{numeric} of \code{length(nrow(x))}, defaults to 0}
\item{ops}{Operators, \code{character} of \code{length(nrow(x))} with the equality operators, defaults to "=="}
\item{...}{further parameters will be given to \code{editmatrix}}
}

