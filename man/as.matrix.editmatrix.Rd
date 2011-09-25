\name{as.matrix.editmatrix}
\alias{as.matrix.editmatrix}
\title{Coerce an editmatrix to a normal matrix}
\usage{
  \method{as.matrix}{editmatrix} (x, ...)
}
\arguments{
  \item{x}{editmatrix object}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  augmented matrix of editmatrix
}
\description{
  An \code{\link{editmatrix}} is a matrix and can be used
  as such, but it has extra attributes. In some cases it is
  preferable to convert the editmatrix to a normal matrix.
  Please note that coercion returns the augmented matrix
  \code{A|b} and not the \code{ops} part.
}

