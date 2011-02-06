\name{violatedEdits}
\alias{violatedEdits}
\title{Retrieve which rows of data...}
\usage{violatedEdits(E, dat, ...)
}
\description{Retrieve which rows of \code{data.frame dat} violate which constraints}
\details{This is an S3 generic function for checking rows of a \code{data.frame} against
a number of edit restrictions. The edits can be entered either in \code{character}
\code{data.frame} or \code{editmatrix} format. The returned value is a logical matrix
with dimension (number of records )\eqn{times}(number of edits), indicating which
record violates (\code{TRUE}) which edit.

This function can be used as an input for automatic corrections methods.
This method will fail if \code{E} contains variables that are not available in \code{dat}}
\alias{violatedEdits.character}
\alias{violatedEdits.data.frame}
\alias{violatedEdits.editmatrix}
\seealso{\code{\link{listViolatedEdits}}, \code{\link{checkRows}}}
\value{a logical matrix where each row indicates which contraints are violated}
\arguments{\item{E}{\code{\link{editmatrix}} containing the constraints for \code{dat}}
\item{dat}{\code{data.frame} with data that should be checked}
\item{...}{further arguments that can be used by methods implementing this generic function}
}
\examples{# Using character vector to define contraints
E <- editmatrix(c( "x+3*y==2*z"
                  , "x==z"
                  )
                )
                
dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)
				 
violatedEdits(E,dat)}

