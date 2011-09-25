\name{checkDatamodel}
\alias{checkDatamodel}
\alias{checkDatamodel.editarray}
\alias{checkDatamodel.editmatrix}
\title{Check data against a datamodel}
\usage{
  checkDatamodel(E, dat, weight = rep(1, ncol(dat)), ...)

  \method{checkDatamodel}{editmatrix} (E, dat,
    weight = rep(1, ncol(dat)), ...)

  \method{checkDatamodel}{editarray} (E, dat,
    weight = rep(1, ncol(dat)), ...)
}
\arguments{
  \item{E}{an object of class \code{\link{editarray}} or
  \code{\link{editmatrix}}}

  \item{dat}{a \code{data.frame}}

  \item{weight}{vector of weigths for every variable of
  \code{dat} or an array of weight of the same dimensions
  as \code{dat}.}

  \item{...}{arguments to be passed to or from other
  methods}
}
\value{
  An object of class \code{\link{errorLocation}}
}
\description{
  Variables in \code{dat} which also occur in \code{E} are
  checked against the datamodel for those variables.
  Currently, this function is only usefull for categorical
  data (i.e. objects of class \code{\link{editarray}}

  Currently, this method returns \code{NULL}, and is only
  used internally.

  In an \code{\link{editarray}}, each row codes a
  multivariate edit, while the columns define the
  datamodel. This function checks if every variable,
  occuring in \code{dat} takes values in the datamodel of
  \code{E}. The function is used by
  \code{\link{localizeErrors}} prior to performing error
  localization on multivariate edits.
}

