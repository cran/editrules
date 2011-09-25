\name{errorLocalizer}
\alias{errorLocalizer}
\alias{errorLocalizer.editarray}
\alias{errorLocalizer.editmatrix}
\title{Localize errors in numerical data based on the paradigm of Fellegi and Holt.}
\usage{
  errorLocalizer(E, x, ...)

  \method{errorLocalizer}{editmatrix} (E, x, weight =
  rep(1, length(x)), maxadapt = length(x), maxweight =
  sum(weight), maxduration = 600, ...)

  \method{errorLocalizer}{editarray} (E, x, weight = rep(1,
  length(x)), maxadapt = length(x), maxweight =
  sum(weight), maxduration = 600, ...)
}
\arguments{
  \item{E}{an \code{\link{editmatrix}} or an
  \code{\link{editarray}}}

  \item{x}{a named numerical vecor (if E is an editmatrix)
  or a named character vector (if E is an editarray). This
  is the record for which errors will be localized.}

  \item{...}{Arguments to be passed to other methods (e.g.
  reliability weights)}

  \item{weight}{a \code{lengt(x)} positive weight vector}

  \item{maxadapt}{maximum number of variables to adapt}

  \item{maxweight}{maximum weight of solution, if weights
  are not given, this is equal to the maximum number of
  variables to adapt.}

  \item{maxduration}{maximum time (in seconds), for
  \code{$searchNext()}, \code{$searchAll()} and
  \code{$searchBest()}}
}
\value{
  an object of class \code{\link{backtracker}}. Each
  execution of \code{$searchNext()} yields a solution in
  the form of a \code{list} (see details). Executing
  \code{$searchBest()} returns the lowest-weight solution.
  When multiple solotions with the same weight are found,
  \code{$searchBest()} picks one at random.
}
\description{
  Localize errors in a record based on Fellegi and Holt's
  paradigm

  Localize errors in numerical data

  Localize errors in categorical data
}
\details{
  Returns a \code{\link{backtracker}} object for error
  localization in numerical data. The returned backtracker
  containts methods to search depth-first to the least
  weighted number of variables that need to be adapted so
  that all restrictions in E can be satisfied. (Generalized
  principle of Fellegi and Holt (1976)).

  The search is excecuted with a branch-and-bound
  algorithm, where in the left branche, a variable is
  assumed correct and its value subsituted in \code{E},
  while in the right branche a variable is assumed
  incorrect and eliminated from \code{E} with
  Fourier-Motzkin elimination. See De Waal (2003), chapter
  8 for a consice description.

  Every call to \code{<backtracker>$searchNext()} returns
  one solution \code{list}, consisting of \itemize{
  \item{w: The solution weight.} \item{adapt:
  \code{logical} indicating whether a variable should be
  adapted (\code{TRUE}) or not}}

  Every subsequent call leads either to \code{NULL}, in
  which case either all solutions have been found, or
  \code{maxduration} was exceeded. The property
  \code{<backtracker>$maxdurationExceeded} indicates if
  this is the case. Otherwise, a new solution with a weight
  \code{w} not higher than the weight of the last found
  solution is returned.

  Alternatively \code{<backtracker>$searchBest()} will
  return the best solution found within \code{maxduration}
  seconds. If multiple equivalent solutions are found, a
  random one is returned.

  The backtracker is prepared such that missing data in the
  input record \code{x} is already set to adapt, and
  missing variables have been eliminated already.
}
\examples{
#### examples with numerical edits
# example with a single editrule
# p = profit, c = cost, t = turnover
E <- editmatrix(c("p + c == t"))
cp <- errorLocalizer(E, x=c(p=755, c=125, t=200))
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
cp <- errorLocalizer(E, x=c(p=755, c=125, t=200), weight=c(1,1,2))
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
cp <- errorLocalizer(E,x=c(p=755,c=125,t=200))
# Now, there's only one solution, but we need two runs to find it (the 1st one has higher weight)
cp$searchNext()
cp$searchNext()

# With the searchBest() function, the lowest weifght solution is found at once:
errorLocalizer(E,x=c(p=755,c=125,t=200))$searchBest()


# An example with missing data.
E <- editmatrix(c(
    "p + c1 + c2 == t",
    "c1 - 0.3*t >= 0",
    "p > 0",
    "c1 > 0",
    "c2 > 0",
    "t > 0"))
cp <- errorLocalizer(E,x=c(p=755, c1=50, c2=NA,t=200))
# (Note that e2 is violated.)
# There are two solutions. Both demand that c2 is adapted:
cp$searchNext()
cp$searchNext()

##### Examples with categorical edits
# 
# 3 variables, recording age class, position in household, and marital status:
# We define the datamodel and the rules
E <- editarray(c(
    "age \%in\% c('under aged','adult')",
    "maritalStatus \%in\% c('unmarried','married','widowed','divorced')",
    "positionInHousehold \%in\% c('marriage partner', 'child', 'other')",
    "if( age == 'under aged' ) maritalStatus == 'unmarried'",
    "if( maritalStatus \%in\% c('married','widowed','divorced')) !positionInHousehold \%in\% c('marriage partner','child')"
    )
)
E

# Let's define a record with an obvious error:
r <- c(age = 'under aged', maritalStatus='married', positionInHousehold='child')
# The age class and position in household are consistent, while the marital status conflicts. 
# Therefore, changing only the marital status (in stead of both age class and postition in household)
# seems reasonable. 
el <- errorLocalizer(E,r)
el$searchNext()










}
\references{
  I.P. Fellegi and D. Holt (1976). A systematic approach to
  automatic edit and imputation. Journal of the American
  Statistical Association 71, pp 17-25

  T. De Waal (2003) Processing of unsave and erroneous
  data.  PhD thesis, Erasmus Research institute of
  management, Erasmus university Rotterdam.
  http://www.cbs.nl/nl-NL/menu/methoden/onderzoek-methoden/onderzoeksrapporten/proefschriften/2008-proefschrift-de-waal.htm
}
\seealso{
  \code{\link{localizeErrors}}
}

