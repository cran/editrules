\name{checkRows}
\alias{checkRows}
\title{Check if rows of a data...}
\usage{checkRows(edtmatrix, dat)
}
\description{Check if rows of a \code{data.frame} violate contraints}
\details{This function can be used to seperate the valid rows/observations from the invalid ones.
This method will fail if \code{edtmatrix} contains variables that are not available in \code{dat}}
\seealso{errorMatrix}
\value{a logical vector with \code{length} equal to \code{nrow(dat)}. If a row is valid is \code{TRUE} otherwise \code{FALSE}}
\arguments{\item{edtmatrix}{\code{\link{editmatrix}} containing the constraints for \code{dat}}
\item{dat}{\code{data.frame} with data that should be checked}
}
\examples{# Using character vector to define contraints
em <- editmatrix(editrules=c( "x+3*y==2*z"
                            , "x==z"
                            )
                )
print(em)

dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
checkRows(em, dat)}

