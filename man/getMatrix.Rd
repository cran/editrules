\name{getMatrix}
\alias{getMatrix}
\title{Returns the coefficient matrix of linear (in)equalities...}
\usage{getMatrix(E)
}
\description{Returns the coefficient matrix of linear (in)equalities}
\seealso{\code{\link{editmatrix}}}
\value{\code{numeric} vector \code{C}}
\arguments{\item{E}{editmatrix}
}
\examples{E <- editmatrix(c( "x+3*y == 2*z"
                 , "x > 2")
                 )
print(E)
                 
# get editrules, useful for storing and maintaining the rules external from your script
editrules(E)
                 
# get constants of inequalities (i.e. c(0, 2))                
getC(E)

# get operators of inequalities (i.e. c("==",">"))
getOps(E)

# get variables of inequalities (i.e. c("x","y","z"))
getVars(E)}

