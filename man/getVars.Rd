\name{getVars}
\alias{getVars}
\title{Returns the variable names of an (in)equality editmatrix E...}
\usage{getVars(E)
}
\description{Returns the variable names of an (in)equality \code{editmatrix} E}
\seealso{\code{\link{editmatrix}}}
\value{\code{character} vector with the names of the variables.}
\arguments{\item{E}{editmatrix}
}
\examples{E <- editmatrix(c( "x+3*y == 2*z"
                 , "x > 2")
                 )
print(E)
                 
# get editrules, useful for storing and maintaining the rules external from your script
editrules(E)
                 
# get coeficient matrix of inequalities
getA(E)

# get augmented matrix of linear edit set
getAb(E)

# get constants of inequalities (i.e. c(0, 2))                
getb(E)

# get operators of inequalities (i.e. c("==",">"))
getOps(E)

# get variables of inequalities (i.e. c("x","y","z"))
getVars(E)

# isNormalized
isNormalized(E)

#normalized E
E <- normalize(E)
E

# is het now normalized?
isNormalized(E)}

