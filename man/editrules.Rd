\name{editrules}
\alias{editrules}
\title{Retrieve readables editrules from an editmatrix}
\usage{
  editrules(x)
}
\arguments{
  \item{x}{\code{\link{editmatrix}} or \code{matrix}
  object}
}
\value{
  \code{data.frame} with information on all edit rules /
  constraints
}
\description{
  THIS FUNCTION IS OBSOLETE AND WILL BE REMOVED. Use
  \link{as.data.frame} or \code{as.editmatrix} instead.
  \code{editrules} returns a data.frame describing the
  editrules in editmatrix \code{x}. This data.frame can be
  used to store the editrules in a readable format, so that
  the editrules can be maintained and documented.
}
\details{
  The \code{\link{editmatrix}} function can use the output
  of \code{editrules} to create an \code{editmatrix}.

  If \code{x} is a normal matrix, the matrix will be
  coerced to an \code{editmatrix}. The columns of the
  matrix are the variables and the rows are the edit rules
  (constraints).
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

