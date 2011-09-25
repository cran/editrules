\name{reduce}
\alias{reduce}
\alias{reduce.editarray}
\alias{reduce.editmatrix}
\alias{reduce.editset}
\title{Remove empty rows and variables from set of edits}
\usage{
  reduce(E, ...)

  \method{reduce}{editmatrix} (E,
    tol = sqrt(.Machine$double.eps), ...)

  \method{reduce}{editarray} (E, ...)

  \method{reduce}{editset} (E, ...)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{...}{arguments to pass to other methods}

  \item{tol}{elements of \code{E} with absolute value <
  \code{tol} are considered 0.}
}
\description{
  If \code{E} is an \code{\link{editmatrix}} all rows and
  columns containing only zeros are removed. If \code{E} is
  an \code{\link{editarray}} all variables not contained in
  any edit are removed. rows which have \code{FALSE} in
  every category of any variable are removed as well (these
  rows have \code{\link{isObviouslyRedundant}} equal to
  \code{TRUE}).
}

