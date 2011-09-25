\name{createXlim}
\alias{createXlim}
\title{Utility function for generating sensible boundaries for variables
Needed for mip error localization.}
\usage{
  createXlim(x, factor = 1, offset = c(-1000, 1000),
    na.rm = FALSE, maxvalue = 1e+08, minvalue = -maxvalue,
    ...)
}
\arguments{
  \item{x}{\code{data vector}}

  \item{factor}{multiplicative factor for range of x}

  \item{offset}{offset added to range of x}

  \item{na.rm}{\code{logical} If set to \code{TRUE} NA's
  will be treated as zero's, otherwise if x contains NA's
  minvalue and maxvalue will be returned}

  \item{minvalue}{If x contains \code{NA} and na.rm is
  \code{FALSE}, the returned xlim will have minvalue as
  lower boundary}

  \item{maxvalue}{If x contains \code{NA} and na.rm is
  \code{FALSE}, the returned xlim will have maxvalue as
  upper boundary}

  \item{...}{not used}
}
\value{
  a lower and upper boundary of \code{x}
}
\description{
  This function determines the minimum and maximum value in
  \code{x} and applies an offset to it. In case of NA
  values will be treated as zero.
}
\keyword{internal}

