\name{editmatrix}
\alias{editmatrix}
\title{Reading in edit rules}
\usage{editmatrix(editrules=editsinfo, editsinfo)
}
\description{Transforms a list of R (in)equalities into an edit matrix with a factor for each variable}
\details{There are two forms of creating an editmatrix:
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

The matrix is created by getting the factors of the variables in the equalities.
i.e. \code{x == y}   results in  \code{c(x=-1, y=1, w=0, z=0)}
and \code{x == y + w} results in \code{c(x=-1, y=1, w=1, z=0)}}
\seealso{\code{\link{editrules}} \code{\link{as.editmatrix}}}
\value{an object of class "editmatrix" which is a \code{matrix} with extra attributes}
\arguments{\item{editrules}{\code{data.frame} with (in)equalities written in R syntax, see details for description or alternatively 
a \code{character} with (in)equalities written in R syntax}
\item{editsinfo}{deprecated}
}
\examples{# Using character vector to define contraints
em <- editmatrix(c("x+3*y==2*z", "x==z"))
print(em)

#Using data.frame to define constraints
editRules.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'

editRules <- read.csv(textConnection(editRules.csv))			
em <- editmatrix(editRules)
print(em)}

