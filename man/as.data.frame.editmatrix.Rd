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
  data.frame which can be converted to an editmatrix with
  \code{\link{editmatrix}}
}
\description{
  Coerces an editmatrix to a \code{data.frame}. Useful for
  storing manipulated edits. NOTE: since version 2.0-0, the
  behaviour of this function changed to be more symmetrical
  with \code{editmatrix.data.frame} and
  \code{as.data.frame.editarray}. Use
  \code{\link{toDataFrame}} for the old behaviour.
}

