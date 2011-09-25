\name{localizeErrors}
\alias{localizeErrors}
\title{Localize errors on records in a data.frame.}
\usage{
  localizeErrors(E, dat, ...)
}
\arguments{
  \item{E}{an object of class \code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{dat}{a \code{data.frame} with variables in E.}

  \item{...}{options to be passed to
  \code{\link{errorLocalizer}}}
}
\value{
  an object of class \code{errorLocation}
}
\description{
  Loops over all records in \code{dat} and performs error
  localization with \code{\link{errorLocalizer}}. For each
  record it finds the smallest (weighted) number of
  variables to be imputed or adapted such that all violated
  edits can be satisfied, without violating new ones. If
  there are multiple optimal (equally weighted) solutions a
  random solution is chosen.
}
\examples{

# an editmatrix and some data:
E <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0"))

dat <- data.frame(
    x = c(1,-1,1),
    y = c(-1,1,1),
    z = c(2,0,2))

# localize all errors in the data
err <- localizeErrors(E,dat)

# what has to be adapted:
err$adapt
# weight, number of equivalent solutions, timings,
err$status

# an example with categorical variables
E <- editarray(c(
    "age \%in\% c('under aged','adult')",
    "maritalStatus \%in\% c('unmarried','married','widowed','divorced')",
    "positionInHousehold \%in\% c('marriage partner', 'child', 'other')",
    "if( age == 'under aged' ) maritalStatus == 'unmarried'",
    "if( maritalStatus \%in\% c('married','widowed','divorced')) !positionInHousehold \%in\% c('marriage partner','child')"
    )
)
E

#
dat <- data.frame(
    age = c('under aged','adult','adult' ),
    maritalStatus=c('married','unmarried','widowed' ), 
    positionInHousehold=c('child','other','marriage partner')
)
dat
localizeErrors(E,dat)
# the last record of dat has 2 degenerate solutions. Running  the last command a few times
# demonstrates that one of those solutions is chosen at random.

# Increasing the weight of  'positionInHousehold' for example, makes the best solution
# unique again
localizeErrors(E,dat,weight=c(1,1,2))

}

