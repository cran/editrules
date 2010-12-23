\name{errorMatrix}
\alias{errorMatrix}
\title{Check which rows of a data...}
\usage{errorMatrix(edtmatrix, dat)
}
\description{Check which rows of a \code{data.frame} violate which constraints}
\details{This function can be used as an input for automatic corrections methods.
This method will fail if \code{edtmatrix} contains variables that are not available in \code{dat}}
\seealso{listErrors}
\value{a logical matrix where each row indicates which contraints are violated}
\arguments{\item{edtmatrix}{\code{\link{editmatrix}} containing the constraints for \code{dat}}
\item{dat}{\code{data.frame} with data that should be checked}
}
\examples{# Using character vector to define contraints
em <- editmatrix(editrules=c( "x+3*y==2*z"
                            , "x==z"
                            )
                )
                
dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
				 
errorMatrix(em,dat)}

