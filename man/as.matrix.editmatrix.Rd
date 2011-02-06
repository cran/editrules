\name{as.matrix.editmatrix}
\alias{as.matrix.editmatrix}
\title{Coerce an editmatrix to a normal matrix...}
\usage{\method{as.matrix}{editmatrix}(x, ...)
}
\description{Coerce an editmatrix to a normal matrix}
\details{An \code{editmatrix} is a matrix and can be used as such, but it has extra attributes.
In some cases it is preferable to convert the editmatrix to a normal matrix.
Please note that coercion only returns the coefficient matrix part of the editmatrix, not the \code{C} or \code{ops} part.}
\value{coefficient matrix of editmatrix}
\arguments{\item{x}{editmatrix object}
\item{...}{further arguments passed to or from other methods.}
}

