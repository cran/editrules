\name{blocks}
\alias{blocks}
\title{Decompose matrix or edits in independent blocks}
\usage{
  blocks(M, ...)
}
\arguments{
  \item{M}{Object to be decomposed into}

  \item{...}{Arguments to be passed to or from other
  functions/}
}
\value{
  list of independent subobjects of \code{M}.
}
\description{
  For matrices, a list of matrices is returned which direct
  sum to the original matrix. For objects of class
  \code{\link{editmatrix}}, \code{\link{editarray}} or
  \code{\link{editset}}, lists of corresponding objects not
  sharing any variable are returned.
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

