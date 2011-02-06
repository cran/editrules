\name{blocks}
\alias{blocks}
\title{Break a matrix into blocks...}
\usage{blocks(M)
}
\description{Break a matrix into blocks}
\alias{findBlocks}
\value{list with seperate matrix blocks}
\arguments{\item{M}{\code{matrix} or \code{editmatrix} that will be broken in blocks}
}
\examples{# three seperate blocks, will they be found?
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
blocks(E)}

