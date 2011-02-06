\name{cp.editmatrix}
\alias{cp.editmatrix}
\title{Localize errors in numerical data based on the paradigm of Fellegi and Holt.}
\usage{cp.editmatrix(E, x, weight=rep(1, length(x)))
}
\description{Localize errors in numerical data}
\details{Returns a \code{\link{choicepoint}} object for error localization in numerical data.
The returned choicepoint containts methods to search depth-first to the least weighted
number of variables that need to be adapted so that all restrictions in E can be 
satisfied. (Generalized principle of Fellegi and Holt (1976)).

The search is excecuted with a branch-and-bound algorithm, where in the left branche,
a variable is assumed correct and its value subsituted in \code{E}, while in the right
branche a variable is assumed incorrect and eliminated from \code{E} with Fourier-Motzkin
elimination. See De Waal (2003), chapter 8 for a consice description.

Every call to \code{<choicepoint>$searchNext()} returns one solution \code{list}, consisting of
\itemize{
\item{w: The solution weight.} 
\item{adapt: \code{logical} indicating whether a variable should be adapted (\code{TRUE}) or not}
\item{E: The \code{\link{editmatrix}} with all variables to adapt eliminated}}

Every subsequent call leads either to \code{NULL}, in which case all solutions have been found,
or a new solution with a weight \code{w} not higher than the weight of the last found solution.

The choicepoint is prepared such that missing data in the input record \code{x} is already
set to adapt, and missing variables have been eliminated already.}
\value{an object of class \code{\link{choicepoint}}. Each execution of \code{$searchNext()} yields a solution
in the form of a \code{list} (see details).}
\references{I.P. Fellegi and D. Holt (1976). A systematic approach to automatic edit and imputation. 
Journal of the American Statistical Association 71, pp 17-25

T. De Waal (2003) Processing of unsave and erroneous data.  PhD thesis, Erasmus Research institute 
of management, Erasmus university Rotterdam. 
http://www.cbs.nl/nl-NL/menu/methoden/onderzoek-methoden/onderzoeksrapporten/proefschriften/2008-proefschrift-de-waal.htm}
\arguments{\item{E}{an \code{\link{editmatrix}}}
\item{x}{a named numerical vecor. The record for which errors will be localized.}
\item{weight}{a weight vector of length x}
}
\examples{# example with a single editrule
# p = profit, c = cost, t = turnover
E <- editmatrix(c("p + c == t"))
cp <- cp.editmatrix(E, x=c(p=755, c=125, t=200))
# x obviously violates E. With all weights equal, changing any variable will do.
# first solution:
cp$searchNext()
# second solution:
cp$searchNext()
# third solution:
cp$searchNext()
# there are no more solution since changing more variables would increase the weight,
# so the result of the next statement is NULL:
cp$searchNext()

# Increasing the reliability weight of turnover, yields 2 solutions:
cp <- cp.editmatrix(E, x=c(p=755, c=125, t=200), weight=c(1,1,2))
# first solution:
cp$searchNext()
# second solution:
cp$searchNext()
# no more solutions available:
cp$searchNext()


# A case with two restrictions. The second restriction demands that
# c/t >= 0.6 (cost should be more than 60% of turnover)
E <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0"))
cp <- cp.editmatrix(E,x=c(p=755,c=125,t=200))
# Now, there's only one solution. 
cp$searchNext()
# Note that the editmatrix in the solution object suggests the actual unique value.


# An example with missing data.
E <- editmatrix(c(
    "p + c1 + c2 == t",
    "c1 - 0.3*t >= 0",
    "p > 0",
    "c1 > 0",
    "c2 > 0",
    "t > 0"))
cp <- cp.editmatrix(E,x=c(p=755, c1=50, c2=NA,t=200))
# (Note that e2 is violated.)
# There are two solutions. Both demand that c2 is adapted:
cp$searchNext()
cp$searchNext()}

