\name{toDataFrame}
\alias{toDataFrame}
\title{Coerce an editmatrix to a \code{data.frame}}
\usage{
  toDataFrame(x)
}
\arguments{
  \item{x}{\code{\link{editmatrix}} object}
}
\value{
  data.frame with the coefficient matrix representation of
  \code{x}, an operator column and CONSTANT column.
}
\description{
  Coerces an editmatrix to a \code{data.frame}. Useful for
  viewing the matrix representation of editrules.
}

