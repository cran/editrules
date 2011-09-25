\name{isNormalized}
\alias{isNormalized}
\title{Check if an editmatrix is normalized}
\usage{
  isNormalized(E)
}
\arguments{
  \item{E}{\code{\link{editmatrix}}}
}
\value{
  TRUE if editmatrix was normalized or created with
  \code{normalize=TRUE}
}
\description{
  Check if an editmatrix is normalized
}
\examples{

E <- editmatrix(c( "x+3*y == 2*z"
                 , "x > 2")
                 )
print(E)
                 
# get editrules, useful for storing and maintaining the rules external from your script
as.data.frame(E)
                 
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
isNormalized(E)
}
\seealso{
  \code{\link{editmatrix}}
}

