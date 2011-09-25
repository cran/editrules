\name{editmatrix}
\alias{editmatrix}
\title{Create an editmatrix}
\usage{
  editmatrix(editrules, normalize = TRUE)
}
\arguments{
  \item{editrules}{\code{data.frame} with (in)equalities
  written in R syntax, see details for description or
  alternatively a \code{character} or \code{expression}
  with (in)equalities written in R syntax}

  \item{normalize}{\code{logical} specifying if all edits
  should be transformed (see description)}
}
\value{
  an object of class "editmatrix" which is a \code{matrix}
  with extra attributes
}
\description{
  Transforms a list of R (in)equalities into an edit
  matrix.
}
\details{
  Transforms a list of R (in)equalities into an edit matrix
  with coefficients (\code{A}) for each variable, and a
  constant (\code{b}) and operator (\code{ops}) for each
  edit rule.

  Each row in the resulting editmatrix represents an linear
  (in) equality. Each column in the resulting editmatrix
  represents a variable.

  There are three forms of creating an editmatrix:
  \enumerate{ \item a \code{character} vector with
  (in)equalities written in R syntax \item a
  \code{expression} vector with (in)equalities written in R
  syntax \item a \code{data.frame} with three columns:
  \itemize{ \item name = a \code{character} with the name
  of each rule \item edit = a \code{character} with
  (in)equalities written in R syntax \item description = a
  \code{character} describing the intention of the rule }
  Typically these rules are stored in a external csv file
  (or database). }

  Numerical edits can also be read from a free-form file
  using \code{\link{editfile}}. The function
  \code{\link{as.data.frame}} extracts the third form,
  which can be used to store edit rules externally or to
  recreate an editmatrix later on. Functions
  \code{\link{as.character}} and
  \code{\link{as.expression}} extract the first and second
  form.

  The matrix is created by retrieving the coefficients of
  the variables in the equalities. i.e. \code{x == y}
  results in \code{c(x=1, y=-1)} and \code{x == y + w}
  results in \code{c(x=1, y=-1, w=-1)}

  By default the editmatrix is created using the comparison
  operators (\code{==,<=,>=,<,>}) in the edits. If option
  \code{normalize=TRUE} is used all edits are transformed
  into an A == b, A < b or A <= b form, so that in the
  specification of the edit rules all inequalities can be
  mixed, but the resulting matrix has similar sign.
}
\examples{
# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)

# Using a expression vector to define contraints
E <- editmatrix(expression(x+3*y==2*z, x==z))
print(E)

# an editmatrix also has a summary method:
summary(E)

# select rows from an editmatrix:
E <- editmatrix(c("x+3*y==2*z", "x >= z"))
E[getOps(E) == "=="]


#Using data.frame to define constraints
E.df <- data.frame(
    name =c("A","B","C"),
    edit = c("x == y",    
            "z + w == y + x",
            "z == y + 2*w"),
    description = c(
            "these variables should be equal","","")

)
print(E.df)

E <- editmatrix(E.df)
print(E)
}
\seealso{
  \code{\link{as.editmatrix}}
}

