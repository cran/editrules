\name{substValue}
\alias{substValue}
\title{Reduce an editmatrix by substituting a variable...}
\usage{substValue(E, var, value, remove=FALSE)
}
\description{Reduce an editmatrix by substituting a variable}
\details{Given a set of linear restrictions \eqn{E: {\bf Ax}\odot {\bf b}} with \eqn{\odot\in\{<,\leq,==\}},
and matrix \eqn{{\bf A}} with columns \eqn{{\bf a}_1,{\bf a}_2,\ldots,{\bf a}_n}.
Substituting variable \eqn{x_j} with a value \eqn{\tilde{\bf x}_j} means setting \eqn{{\bf a}_j=0}
and \eqn{{\bf b}={\bf a}_j\tilde{x}_j}.

Note that the resulting \code{\link{editmatrix}} may be inconsistent because of inconsistencies in
\eqn{\tilde{\bf x}}.}
\value{reduced edit matrix}
\arguments{\item{E}{\code{editmatrix} object}
\item{var}{\code{character} with name(s) of variable(s)}
\item{value}{\code{numeric} with value(s) of variable(s)}
\item{remove}{\code{logical} should variable columns be removed from editmatrix?}
}

