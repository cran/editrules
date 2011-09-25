\name{disjunct}
\alias{disjunct}
\title{Decouple a set of conditional edits}
\usage{
  disjunct(E)
}
\arguments{
  \item{E}{an object of class \code{\link{editset}}}
}
\value{
  A list of editsets.  Each element has an attribute
  'condition' showing which conditions were assumed to
  derive the editset.
}
\description{
  An editset is transformed to a list of
  \code{\link{editset}s} which do not contain any
  conditional numeric/categorical edits anymore. Each
  editset has an extra attribute \code{condition}, which
  holds the series of assumptions made to decouple the
  original edits. This attribute will be printed when not
  \code{NULL}.
}
\details{
  At the moment this functionality is somewhat experimental
  and in- or output specification should be expected to
  change in coming releases.
}
\examples{


E <- editset(expression(
    x + y == z,
    if ( x > 0 ) y > 0,
    x >= 0,
    y >= 0,
    z >= 0,
    A \%in\% letters[1:4],
    B \%in\% letters[1:4],
    if (A \%in\% c('a','b')) y > 0,
    if (A == 'c' ) B \%in\% letters[1:3]
))

disjunct(E)




}

