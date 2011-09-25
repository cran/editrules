\name{isObviouslyRedundant}
\alias{isObviouslyRedundant}
\alias{isObviouslyRedundant.editarray}
\alias{isObviouslyRedundant.editmatrix}
\alias{isObviouslyRedundant.matrix}
\title{Find obvious redundancies in set of edits}
\usage{
  isObviouslyRedundant(E, duplicates = TRUE, ...)

  \method{isObviouslyRedundant}{matrix} (E, operators,
    tol = sqrt(.Machine$double.eps), ...)

  \method{isObviouslyRedundant}{editmatrix} (E,
    duplicates = TRUE, ...)

  \method{isObviouslyRedundant}{editarray} (E,
    duplicates = TRUE, ...)
}
\arguments{
  \item{E}{Augmented matrix A|b, editmatrix}

  \item{duplicates}{\code{logical}: check for duplicate
  edits?}

  \item{...}{parameters to be passed to or from other
  methods.}

  \item{operators}{character vecor of comparison operators
  in \code{<, <=, ==} of length \code{nrow(E)}}

  \item{tol}{tolerance to check for zeros.}
}
\value{
  logical vector indicating which edits are (obviously)
  redundant
}
\description{
  Find obvious redundancies in set of edits

  Lower-level method, to be called directly from internal
  functions of the editrules package.

  Normalizes the editmatrix if necessary, converts it and
  passes it to the \code{matrix} method. All options of the
  \code{matrix} method may be passed. Obvious redundancies,
  amounting to statements as 0==0 or 0 < 1 will be
  detected, as well as duplicates.

  Check if any of the variables has FALSE for every
  category (a record can never be contained in such a set).
}
\seealso{
  \code{\link{isObviouslyRedundant}},
  \code{\link{isObviouslyRedundant.editmatrix}}
}
\keyword{internal}

