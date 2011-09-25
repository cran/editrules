\name{blockIndex}
\alias{blockIndex}
\title{Determine list of indices indicating blocks in logical matrix}
\usage{
  blockIndex(D)
}
\arguments{
  \item{D}{matrix of type \code{logical}}
}
\value{
  \code{list} of row indices in \code{D} indicating
  independent blocks.  Empty rows (i.e. every column
  \code{FALSE}) are ignored.
}
\description{
  Determine list of indices indicating blocks in logical
  matrix
}
\examples{
# three seperate blocks, will they be found?
E <- editmatrix( c( "x1 + x2 == x3"
                  , "x3 + x4 == x5"
                  , "x5 + x6 == x7"
                  , "y1 + y2 == y3"
                  , "z1 + z2 == z3")
               )
blocks(E)

# four seperate blocks, will they be found?
E <- editmatrix( c( "x1 + x2 == x3"
                  , "x3 + x4 == x5"
                  , "x8 + x6 == x7"
                  , "y1 + y2 == y3"
                  , "z1 + z2 == z3")
               )
blocks(E)

# two categorical blocks, will they be found?
E <- editarray(c(
 "x \\\%in\\\% c('a','b','c')",
 "y \\\%in\\\% c('d','e')",
 "z \\\%in\\\% c('f','g')",
 "u \\\%in\\\% c('w','t')",
 "if ( x == 'a') y != 'd'",
 "if ( z == 'f') u != 'w'")
)
blocks(E)


}

