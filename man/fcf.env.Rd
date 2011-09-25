\name{fcf.env}
\alias{fcf.env}
\title{Field code forest algorithm}
\usage{
  fcf.env(E, vars, env = new.env())
}
\arguments{
  \item{E}{an editarray}

  \item{vars}{variable names still to be eliminated from E}

  \item{env}{an environment where all editmatrices will be
  stored}
}
\description{
  Workhorse function for \code{\link{fcf}}
}
\seealso{
  \code{\link{fcf}}
}
\keyword{internal}

