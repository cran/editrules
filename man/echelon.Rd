\name{echelon}
\alias{echelon}
\title{Bring an (edit) matrix to reduced row echelon form.}
\usage{echelon(E, ...)
}
\description{Bring an (edit) matrix to reduced row echelon form.}
\details{If E is a matrix, a matrix in reduced row echelon form is returned.
If E is an \code{\link{editmatrix}} the equality part of E is transformed
to reduced row echelon form.}
\alias{echelon.editmatrix}
\alias{echelon.matrix}
\arguments{\item{E}{a matrix or editmatrix}
\item{...}{options to pass on to further methods.}
}

