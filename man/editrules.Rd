\name{editrules}
\alias{editrules}
\title{Retrieve editrules from an editmatrix...}
\usage{editrules(x)
}
\description{Retrieve editrules from an editmatrix}
\details{\code{editrules} returns a data.frame describing the editrules in editmatrix \code{x}. This data.frame can be used to store the
editrules in a readable format, so that the editrules can be maintained and documented.

The \code{\link{editmatrix}} function can use the output of \code{editrules} to create an \code{editmatrix}.

If \code{x} is a normal matrix, the matrix will be considered an \code{editmatrix}. The columns of the matrix
are the variables and the rows are the edit rules (constraints).}
\alias{editrules}
\alias{editsinfo}
\seealso{\code{\link{editmatrix}}}
\value{\code{data.frame} with information on all edit rules / constraints}
\arguments{\item{x}{\code{\link{editmatrix}}  or \code{matrix} object}
}

