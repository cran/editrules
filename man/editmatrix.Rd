\name{editmatrix}
\alias{editmatrix}
\title{Create an editmatrix}
\usage{editmatrix(editrules, normalize=TRUE)
}
\description{Transforms a list of R (in)equalities into an edit matrix.}
\details{Transforms a list of R (in)equalities into an edit matrix with coefficients (\code{A}) for each variable, and a constant (\code{b})
and operator (\code{ops}) for each edit rule.

Each row in the resulting editmatrix represents an linear (in) equality.
Each column in the resulting editmatrix represents a variable.

There are two forms of creating an editmatrix:
\enumerate{ 
\item a \code{character} vector with (in)equalities written in R syntax
\item a \code{data.frame} with three columns:
\itemize{
\item name = a \code{character} with the name of each rule
\item edit = a \code{character} with (in)equalities written in R syntax
\item description = a \code{character} describing the intention of the rule
}
Typically these rules are stored in a external csv file (or database). 
}

The second form is the prefered form, because it allows the documentation of constraints. This
may be very useful when the incorrect observations are analyzed.
If the first form is used, \code{editmatrix} internally creates the second form. This information
can be retrieved by using \code{\link{editrules}}

The matrix is created by retrieving the coefficients of the variables in the equalities.
i.e. \code{x == y}   results in  \code{c(x=1, y=-1)}
and \code{x == y + w} results in \code{c(x=1, y=-1, w=-1)}

By default the editmatrix is created using the comparison operators (\code{==,<=,>=,<,>}) in the edits. If option \code{normalize=TRUE} is used all 
edits are transformed into an A == b, A < b or A <= b form, so that in the specification of the edit rules all inequalities can be mixed, 
but the resulting matrix has similar sign.}
\seealso{\code{\link{editrules}} \code{\link{as.editmatrix}}}
\value{an object of class "editmatrix" which is a \code{matrix} with extra attributes}
\arguments{\item{editrules}{\code{data.frame} with (in)equalities written in R syntax, see details for description or alternatively 
a \code{character} with (in)equalities written in R syntax}
\item{normalize}{\code{logical} specifying if all edits should be transformed (see description)}
}
\examples{# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)

# select rows from an editmatrix:
E <- editmatrix(c("x+3*y==2*z", "x >= z"))
E[getOps(E) == "=="]


#Using data.frame to define constraints
E.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'
E.df <- read.csv(textConnection(E.csv))			
print(E.df)

E <- editmatrix(E.df)
print(E)}

