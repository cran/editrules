\name{listErrors}
\alias{listErrors}
\title{Lists which rows of a data...}
\usage{listErrors(edtmatrix, dat)
}
\description{Lists which rows of a \code{data.frame} violate which constraints}
\details{This function can be used as an input for automatic corrections methods.}
\seealso{errorMatrix}
\value{a list where per row a \code{integer} vector of the constraints that are violated}
\arguments{\item{edtmatrix}{\code{\link{editmatrix}} containing the constraints for \code{dat}}
\item{dat}{\code{data.frame} with data that should be checked}
}
\examples{# Using character vector to define contraints
em <- editmatrix(editrules=c("x+3*y==2*z", "x==z"))
print(em)

dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
valid <- checkRows(em, dat)
invalid <- dat[!valid,] 
listErrors(em,invalid)}

