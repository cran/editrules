\name{eliminateFM}
\alias{eliminateFM}
\title{Eliminate a variable from an editmatrix...}
\usage{eliminateFM(E, var, fancynames=FALSE)
}
\description{Eliminate a variable from an editmatrix}
\details{Uses Fourier-Motzkin elimination to eliminate a variable from a set
of linear (in)equality restrictions, represented as an \code{\link{editmatrix}}.
An observation of Kohler (1967) is used to reduce the number of implied 
restrictions. Obvious redundancies of the type 0 < 1 are removed as well.}
\value{An editmatrix with an extra (hidden) attributes describing how the new restrictions were derived from the original ones.
These attributes are used to remove redundancies when variables are repeatedly eliminated.}
\seealso{\code{\link{editmatrix}} \code{\link{isObviouslyInfeasible}}}
\references{D.A. Kohler (1967) Projections of convex polyhedral sets, Operational Research
Center Report , ORC 67-29, University of California, Berkely.

H.P. Williams (1986) Fourier's method of linear programming and its dual,
The American Mathematical Monthly 93, 681-695}
\arguments{\item{E}{an object of class \code{\link{editmatrix}}}
\item{var}{\code{character} name of the variable to eliminate}
\item{fancynames}{\code{logical} If true, the derived restrictions have rownames derived from the original restrictions (slower).}
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
(P1 <- eliminateFM(P, "x1", fancynames=TRUE))
# eliminate 2nd variable. Note that redundant rows have been eliminated
(P2 <- eliminateFM(P1, "x2", fancynames=TRUE))
# finally, the answer:
(P3 <- eliminateFM(P2, "x3", fancynames=TRUE))

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
eliminateFM(E,"x", fancynames=TRUE)


# This example has two equalities, and it's solution 
# is the origin (x,y)=(0,0)
(E <- editmatrix(c(
    "y <= 1 - x",
    "y >= -1 + x",
    "x == y",
    "y ==-2*x" ),normalize=TRUE))
eliminateFM(E,"x", fancynames=TRUE)

# this example has no solution, the equalities demand (x,y) = (0,2)
# while the inequalities demand y <= 1
(E <- editmatrix(c(
    "y <= 1 - x",
    "y >= -1 + x",
    "y == 2 - x",
    "y == -2 + x" ),normalize=TRUE))
# this happens to result in an obviously unfeasable system:
isObviouslyInfeasible(eliminateFM(E,"x"))}

