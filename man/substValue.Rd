\name{substValue}
\alias{substValue}
\alias{substValue.editarray}
\alias{substValue.editmatrix}
\title{Replace a variable by a value in a set of edits.}
\usage{
  substValue(E, var, value, ...)

  \method{substValue}{editmatrix} (E, var, value, reduce =
  FALSE, ...)

  \method{substValue}{editarray} (E, var, value, reduce =
  FALSE, ...)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{var}{\code{character} with name(s) of variable(s)
  to substitute}

  \item{value}{vector with value(s) of variable(s)}

  \item{...}{arguments to be passed to or from other
  methods}

  \item{reduce}{\code{logical} should variable columns be
  removed from editmatrix?}
}
\value{
  \code{E}, with variables replaced by values
}
\description{
  Replace a variable by a value in a set of edits.

  Given a set of linear restrictions \eqn{E: {\bf Ax}\odot
  {\bf b}} with \eqn{\odot\in\{<,\leq,==\}}, and matrix
  \eqn{{\bf A}} with columns \eqn{{\bf a}_1,{\bf
  a}_2,\ldots,{\bf a}_n}. Substituting variable \eqn{x_j}
  with a value \eqn{\tilde{\bf x}_j} means setting
  \eqn{{\bf a}_j=0} and \eqn{{\bf b}={\bf a}_j\tilde{x}_j}.

  Only rows with \code{<var>:<value>==TRUE} are kept. In
  the kept rows, categories not equal to <value> are set to
  \code{FALSE} Multiple replacements is not yet
  implemented.
}
\details{
  Note that the resulting \code{\link{editmatrix}} may be
  inconsistent because of inconsistencies in
  \eqn{\tilde{\bf x}}.
}

