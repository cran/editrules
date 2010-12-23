\name{editmatrix}
\alias{editmatrix}
\title{Reading in edit rules}
\usage{editmatrix(editsinfo, editrules)
}
\description{Transforms a list of R (in)equalities into an edit matrix with a factor for each variable}
\details{There are two forms of creating an editmatrix:
\enumerate{ 
\item a \code{character} vector with (in)equalities written in R syntax
\item a \code{data.frame}(in) with three fields:
\itemize{
\item name = a \code{character} with the name of each rule
\item edit = a \code{character} vector with (in)equalities written in R syntax
\item description = a \code{character} desribing the intention of the rule
}
Typically these rules are stored in a external csv file (or database). 
}

The second form is the prefered form, because it allows the documentation of constraints. This
may be very useful when the incorrect observations are analyzed. 

The matrix is created by getting the factors of the variables in the equalities.
i.e. \code{x == y}   results in  \code{c(x=-1, y=1, w=0, z=0)}
and \code{x == y + w} results in \code{c(x=-1, y=1, w=1, z=0)}}
\value{an object of class "editmatrix" which is a \code{matrix} with extra properties}
\arguments{\item{editsinfo}{\code{data.frame} with (in)equalities written in R syntax, see details for description}
\item{editrules}{\code{character} vector with (in)equalities written in R syntax}
}
\examples{# Using character vector to define contraints
em <- editmatrix(editrules=c("x+3*y==2*z", "x==z"))
print(em)

#Using editsinfo to define constraints
edtinf.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'

edtinf <- read.csv(textConnection(edtinf.csv))			
em <- editmatrix(edtinf)
print(em)}

