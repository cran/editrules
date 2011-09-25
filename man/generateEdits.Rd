\name{generateEdits}
\alias{generateEdits}
\title{Derive all essentially new implicit edits}
\usage{
  generateEdits(E)
}
\arguments{
  \item{E}{An \code{\link{editarray}}}
}
\value{
  A 3-element named list \code{list}, where element
  \code{E} is an \code{\link{editarray}} containing all
  generated edits. \code{nodes} contains information on the
  number of nodes in the tree and vs the number of nodes
  traversed and \code{duration} contains user, system and
  elapsed time inseconds. The
  \code{\link{summary.editarray}} method prints this
  information.
}
\description{
  Implements the Field Code Forest (FCF) algorithm of
  Garfinkel et al (1986) to derive all essentially new
  implicit edits from an editarray. The FCF is really a
  single, highly unbalanced tree. This algorithm traverses
  the tree, pruning many unnecessary branches, using
  \code{\link{blocks}} to divide and conquer, and
  optimizing the order of traversing.
}
\references{
  R.S. Garfinkel, A.S. Kunnathur and G.E. Liepins (1986).
  Optimal imputation of erroneous data: categorical data,
  general edits.  Operations Research 34, 744-751. M.P.J.
  Van der Loo Variable elimination and edit generation with
  a flavour of semigroup algebra (in preparation)
}

