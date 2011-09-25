\name{cateditmatrix}
\alias{cateditmatrix}
\title{Create an editmatrix with categorical variables}
\usage{
  cateditmatrix(x, sep = ":", env = parent.frame())
}
\arguments{
  \item{x}{\code{character} with categorical edits}
}
\value{
  cateditmatrix object, which is a specialized
  \code{\link{editmatrix}}
}
\description{
  \code{cateditmatrix} is an alternative representation of
  a categorial edit. The default representation in
  editrules is \code{editarray}, but cateditmatrix is
  useful for transforming and solving categorical edit into
  a mixed integer programming problem
}
\keyword{internal}

