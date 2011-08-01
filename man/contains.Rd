\name{contains}
\alias{contains}
\title{determine which edits in an editmatrix contain a variable.}
\usage{
  contains(E, var = NULL)
}
\arguments{
  \item{E}{\code{\link{editarray}}}

  \item{var}{\code{character}, names of a categorical
  variables in \code{E}. If var=NULL, all variables are
  treated.}
}
\value{
  \code{logical} vector of length nrow(E), TRUE for edits
  containing \code{var}
}
\description{
  determine which edits in an editmatrix contain a
  variable.
}

