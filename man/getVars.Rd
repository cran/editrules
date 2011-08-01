\name{getVars}
\alias{getVars}
\title{get names of variables in a set of edits}
\usage{
  getVars(E)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} or
  \code{\link{editarray}}}
}
\value{
  \code{character} vector with the names of the variables.
}
\description{
  get names of variables in a set of edits
}
\examples{

E <- editmatrix(c( "x+3*y == 2*z"
                 , "x > 2")
                 )
getVars(E)

E <- editarray(c(
    "gender \%in\% c('male','female')",
    "pregnant \%in\% c(TRUE, FALSE)",
    "if( gender == 'male' ) pregnant == FALSE"
    )
)

getVars(E)

}
\seealso{
  \code{\link{getA}}, \code{\link{getb}},
  \code{\link{getOps}}
}

