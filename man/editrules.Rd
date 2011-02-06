\name{editrules}
\alias{editrules}
\title{Retrieve readables editrules from an editmatrix...}
\usage{editrules(x)
}
\description{Retrieve readables editrules from an editmatrix}
\details{\code{editrules} returns a data.frame describing the editrules in editmatrix \code{x}. This data.frame can be used to store the
editrules in a readable format, so that the editrules can be maintained and documented.

The \code{\link{editmatrix}} function can use the output of \code{editrules} to create an \code{editmatrix}.

If \code{x} is a normal matrix, the matrix will be coerced to an \code{editmatrix}. The columns of the matrix
are the variables and the rows are the edit rules (constraints).}
\alias{editrules}
\seealso{\code{\link{editmatrix}}}
\value{\code{data.frame} with information on all edit rules / constraints}
\arguments{\item{x}{\code{\link{editmatrix}}  or \code{matrix} object}
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

