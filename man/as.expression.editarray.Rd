\name{as.expression.editarray}
\alias{as.expression.editarray}
\title{Coerce an editarray to R expressions}
\usage{
  \method{as.expression}{editarray} (x, ...)
}
\arguments{
  \item{x}{editarray object to be parsed}

  \item{...}{further arguments passed to or from other
  methods.}
}
\description{
  Generates an R \code{expression} vector that can be used
  to check data using \code{\link{eval}}.
}

