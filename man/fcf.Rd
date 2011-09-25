\name{fcf}
\alias{fcf}
\title{Derive all essentially new implicit edits}
\usage{
  fcf(E)
}
\arguments{
  \item{E}{An \code{\link{editarray}}}
}
\description{
  Implements the Field Code Forest algorithm of Garfinkel
  et al (1986) to derive all essentially new implicit edits
  from an editarray. At the moment this algorithm has very
  little optimization and can be very slow.
}
\references{
  R.S. Garfinkel, A.S. Kunnathur and G.E. Liepins (1986).
  Optimal imputation of erroneous data: categorical data,
  general edits.  Operations Research 34, 744-751.
}

