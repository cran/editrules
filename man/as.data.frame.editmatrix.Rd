\name{as.data.frame.editmatrix}
\alias{as.data.frame.editmatrix}
\title{Coerce an editmatrix to a \code{data.frame}}
\usage{
  \method{as.data.frame}{editmatrix} (x, ...)
}
\arguments{
  \item{x}{editmatrix object}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  data.frame with the coefficient matrix representation of
  \code{x}, an operator column and CONSTANT column.
}
\description{
  Coerces an editmatrix to a \code{data.frame}. Useful for
  storing/viewing the matrix representation of editrules.
}

