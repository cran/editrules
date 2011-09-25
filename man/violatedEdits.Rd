\name{violatedEdits}
\alias{violatedEdits}
\alias{violatedEdits.character}
\alias{violatedEdits.data.frame}
\alias{violatedEdits.editarray}
\alias{violatedEdits.editmatrix}
\title{Retrieve which rows of \code{data.frame dat} violate which constraints}
\usage{
  violatedEdits(E, dat, ...)

  \method{violatedEdits}{character} (E, dat, name = NULL,
  ...)

  \method{violatedEdits}{editmatrix} (E, dat, tol = 0, ...)

  \method{violatedEdits}{data.frame} (E, dat, ...)

  \method{violatedEdits}{editarray} (E, dat, datamodel =
  TRUE, ...)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} containing the
  constraints for \code{dat}}

  \item{dat}{\code{data.frame} with data that should be
  checked, if a named vector is supplied it will converted
  internally to a data.frame}

  \item{...}{further arguments that can be used by methods
  implementing this generic function}

  \item{name}{name of edits}

  \item{tol}{tolerance to check rules against.}

  \item{datamodel}{Also check against datamodel?}
}
\value{
  a logical matrix where each row indicates which
  contraints are violated
}
\description{
  This is an S3 generic function for checking rows of a
  \code{data.frame} against a number of edit restrictions.
  The edits can be entered either in \code{character}
  \code{data.frame} or \code{editmatrix} format. The
  returned value is a logical matrix with dimension (number
  of records )\eqn{times}(number of edits), indicating
  which record violates (\code{TRUE}) which edit.

  \itemize{ \item{For rules of the form Ax == b |Ax - b| <=
  tol is returned.} \item{For rules of the form Ax < b, Ax
  - b < tol is returned.} \item{For rules of the form Ax <=
  b Ax- b <= tol is returned.} } For numerical records, the
  default tolerance is 0. When working with doubles, the
  square root of machina accuracy is a resonable
  alternative (\code{sqrt(.Machine\$double.eps)}). The
  editmatrix is \cite{\link[=normalize]{normalized}} before
  checks are performed.
}
\details{
  This function can be used as an input for automatic
  corrections methods. This method will fail if \code{E}
  contains variables that are not available in \code{dat}
}
\examples{
# Using character vector to define contraints
E <- editmatrix(c( "x+3*y==2*z"
                  , "x==z"
                  )
                )
                
dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)

ve <- violatedEdits(E,dat)

print(ve)
summary(ve, E)
plot(ve)

# An example with categorical data:

E <- editarray(c(
    "gender \%in\% c('male','female')",
    "pregnant \%in\% c(TRUE, FALSE)",
    "if( gender == 'male' ) !pregnant"
    )
)
print(E)

dat <- data.frame(
    gender=c('male','male','female','cylon'), 
    pregnant=c(TRUE,FALSE,TRUE,TRUE)
)

print(dat)
# Standard, the datamodel is checked as well,
violatedEdits(E,dat)

# but we may turn this of
violatedEdits(E,dat,datamodel=FALSE)


}
\seealso{
  \code{\link{listViolatedEdits}}, \code{\link{checkRows}}
}

