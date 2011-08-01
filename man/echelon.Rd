\name{echelon}
\alias{echelon}
\alias{echelon.editmatrix}
\alias{echelon.matrix}
\title{Bring an (edit) matrix to reduced row echelon form.}
\usage{
  echelon(E, ...)

  \method{echelon}{editmatrix} (E, ...)

  \method{echelon}{matrix} (E, tol =
  sqrt(.Machine$double.eps), ...)
}
\arguments{
  \item{E}{a matrix or editmatrix}

  \item{...}{options to pass on to further methods.}

  \item{tol}{tolerance that will be used to determine if a
  coefficient equals zero.}
}
\description{
  If E is a matrix, a matrix in reduced row echelon form is
  returned. If E is an \code{\link{editmatrix}} the
  equality part of E is transformed to reduced row echelon
  form.

  Write a system of equations in reduced row echelon form
}

