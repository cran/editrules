\name{contains}
\alias{contains}
\alias{contains.editarray}
\alias{contains.editmatrix}
\title{determine which edits in an editmatrix contain a variable.}
\usage{
  contains(E, var = NULL, ...)

  \method{contains}{editmatrix} (E, var = NULL,
    tol = sqrt(.Machine$double.eps), ...)

  \method{contains}{editarray} (E, var = NULL, ...)
}
\arguments{
  \item{E}{\code{\link{editarray}} or
  \code{\link{editmatrix}}}

  \item{var}{\code{character}, names of a categorical
  variables in \code{E}. If var=NULL, all variables are
  treated.}

  \item{...}{arguments to be passed to other methods}

  \item{tol}{tolerance to check zero-entries}
}
\value{
  \code{logical} vector of length nrow(E), TRUE for edits
  containing \code{var}
}
\description{
  For an \code{\link{editmatrix}}, variables with
  coefficients smaller than \code{tol} are considered not
  to be contained in an edit.

  editmatrix method for contains

  editarray method for contains
}
\keyword{internal}

