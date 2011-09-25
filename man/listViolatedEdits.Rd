\name{listViolatedEdits}
\alias{listViolatedEdits}
\title{Lists which rows of \code{data.frame dat} violate which constraints}
\usage{
  listViolatedEdits(E, dat)
}
\arguments{
  \item{E}{a number of edit restrictions, represented as
  \code{character} vector, \code{\link{editmatrix}} or
  \code{data.frame}.}

  \item{dat}{\code{data.frame} with data that should be
  checked}
}
\value{
  a list with per row a \code{integer} vector of the
  constraints that are violated
}
\description{
  This function can be used as an input for automatic
  corrections methods.
}
\examples{
# Using character vector to define contraints
E <- editmatrix(editrules=c("x+3*y==2*z", "x==z"))
print(E)

dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
valid <- checkRows(E, dat)
invalid <- dat[!valid,] 
listViolatedEdits(E,invalid)
}
\seealso{
  \code{\link{violatedEdits}} \code{\link{checkRows}}
}

