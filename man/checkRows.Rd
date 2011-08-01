\name{checkRows}
\alias{checkRows}
\alias{checkRows.character}
\alias{checkRows.data.frame}
\alias{checkRows.editmatrix}
\title{Check rows of data.frame against edits.}
\usage{
  checkRows(E, dat)

  \method{checkRows}{editmatrix} (E, dat)

  \method{checkRows}{character} (E, dat)

  \method{checkRows}{data.frame} (E, dat)
}
\arguments{
  \item{E}{Edits, in \code{character}, \code{data.frame} or
  \code{\link{editmatrix}} representation}

  \item{dat}{The data to check.}
}
\value{
  a logical vector with \code{length} equal to
  \code{nrow(dat)}. If a row is violates no edit
  restrictions, \code{TRUE} otherwise \code{FALSE}
}
\description{
  This is an S3 generic function for checking rows of a
  \code{data.frame} against a number of edit restrictions.
  The edits can be entered either in \code{character}
  \code{data.frame} or \code{editmatrix} format.
}
\details{
  If edits are represented as a \code{character} vector,
  the entries of \code{E} are parsed and evaluated in the
  environment of \code{dat}

  If the edits are represented in a \code{data.frame}, the
  \code{data.frame} must have the format described in
  \code{\link{editmatrix}}. The edits are coerced to a
  character vector, and passed to
  \code{checkRows.character}.

  If the edits are represented by an
  \code{\link{editmatrix}} (representing linear
  (in)equalities) verbose edits are extracted and passed on
  to \code{checkRows.character}
}
\examples{
# Using character vector to define contraints
E <- editmatrix(c( "x+3*y==2*z",
                  "x==z"))
print(E)

dat <- data.frame( x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows? editmatrix method
checkRows(E, dat)


# example using the character method.
data(women)
e <- c("height<=71", "weight/height>=2.1")
valid <- checkRows(e,women)
women[valid,]

# same example, using data.frame method
ef <- data.frame(
    name=c("rule 1", "rule 2"), 
    edit=e, 
    description=c("descr1","descr2"))
checkRows(ef,women)
}
\seealso{
  violatedEdits
}

