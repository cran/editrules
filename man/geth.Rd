\name{geth}
\alias{getH}
\title{Returns the derivation history of an edit matrix or array...}
\usage{getH(E)
geth(E)
}
\description{Returns the derivation history of an edit matrix or array}
\details{\code{getH}: eliminate tracks the history of edits in a logical array H.
H has nrow(E) rows and the number of columns is the number of
edits in the editmatrix/array as it was first defined. If 
H[i,j1], H[i,j2],...,H[i,jn] are TRUE, then E[i,] is some 
(positive, linear) combination of original edits E[j1,], E[j2,],...,E[jn,]

Attributes H and h are used to detect redundant derived edits.

\code{geth}: h records the number of variables eliminated from E by \code{\link{eliminate}}


}
\seealso{\code{\link{editmatrix}}, \code{\link{eliminate}}\code{\link{editmatrix}}, \code{\link{eliminate}}}
\arguments{\item{E}{editmatrix}
}
\examples{# The following is an example by Williams (1986). Eliminating all variables
# except z maximizes -4x1 + 5x2 +3x3:
P <- editmatrix(c(
     "4*x1 - 5*x2 - 3*x3 + z <= 0",
     "-x1 + x2 -x3 <= 2",
     "x1 + x2 + 2*x3 <= 3",
     "-x1 <= 0",
     "-x2 <= 0",
     "-x3 <= 0"))
# eliminate 1st variable
(P1 <- eliminate(P, "x1", fancynames=TRUE))
# eliminate 2nd variable. Note that redundant rows have been eliminated
(P2 <- eliminate(P1, "x2", fancynames=TRUE))
# finally, the answer:
(P3 <- eliminate(P2, "x3", fancynames=TRUE))

# check which original edits were used in deriving the new ones
getH(P3)

# check how many variables were eliminated
geth(P3)


# An  example with an equality and two inequalities
# The only thing to do is solving for x in e1 and substitute in e3.
(E <- editmatrix(c(
    "2*x + y == 1",
    "y > 0",
    "x > 0"),normalize=TRUE))
eliminate(E,"x", fancynames=TRUE)


# This example has two equalities, and it's solution 
# is the origin (x,y)=(0,0)
(E <- editmatrix(c(
    "y <= 1 - x",
    "y >= -1 + x",
    "x == y",
    "y ==-2*x" ),normalize=TRUE))
eliminate(E,"x", fancynames=TRUE)

# this example has no solution, the equalities demand (x,y) = (0,2)
# while the inequalities demand y <= 1
(E <- editmatrix(c(
    "y <= 1 - x",
    "y >= -1 + x",
    "y == 2 - x",
    "y == -2 + x" ),normalize=TRUE))
# this happens to result in an obviously unfeasable system:
isObviouslyInfeasible(eliminate(E,"x"))}
\alias{geth}

