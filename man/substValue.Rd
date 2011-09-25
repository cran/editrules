\name{substValue}
\alias{substValue}
\alias{substValue.editarray}
\alias{substValue.editmatrix}
\alias{substValue.editset}
\title{Replace a variable by a value in a set of edits.}
\usage{
  substValue(E, var, value, ...)

  \method{substValue}{editmatrix} (E, var, value,
    reduce = FALSE, removeredundant = TRUE, ...)

  \method{substValue}{editarray} (E, var, value,
    reduce = FALSE, ...)

  \method{substValue}{editset} (E, var, value,
    simplify = TRUE, ...)
}
\arguments{
  \item{E}{\code{\link{editmatrix}} or
  \code{\link{editarray}}}

  \item{var}{\code{character} with name(s) of variable(s)
  to substitute}

  \item{value}{vector with value(s) of variable(s)}

  \item{...}{arguments to be passed to or from other
  methods}

  \item{reduce}{\code{logical} should substituted variables
  be removed?}

  \item{removeredundant}{\code{logical} should empty rows
  be removed?}

  \item{simplify}{Simplify editset by moving logical edits
  containing a simple numerical statement to the pure
  numerical part?}
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

  For editarrays, only rows with \code{<var>:<value>==TRUE}
  are kept. In the kept rows, categories not equal to
  <value> are set to \code{FALSE} If \code{reduce=TRUE},
  columns corresponding to categories which are set to
  \code{FALSE} will be removed. Note that the function
  \code{\link{reduce}} has a different effect (it removes
  complete variables).

  For an \code{\link{editset}}, purely numerical variables
  are substitutes as in an \code{\link{editmatrix}} and
  categorical as in an \code{\link{editarray}}. Numerical
  variables appearing logical constraints are substituted
  and if truth values can be derived these are substituted
  in the logical constraint.
}
\details{
  Note that the resulting \code{\link{editmatrix}} may be
  inconsistent because of inconsistencies in
  \eqn{\tilde{\bf x}}.
}
\examples{


E <- editmatrix(expression(
    x + y == z,
    2*y < 10,
    3*x + 1.5*u < 7,
    z >= 0
    )
)

# single value
substValue(E,'z',10)
# multiple values
substValue(E,c('x','y'),c(1,3))
# remove substituted variable from edits
substValue(E,'z',10,reduce=TRUE)
# do not remove redundant row:
substValue(E,'z',10,removeredundant=FALSE)


# example with an editset
E <- editset(expression(
    x  + y == z,
    x >= 0,
    y >= 0,
    A \%in\% c('a1','a2'),
    B \%in\% c('b1','b2'),
    if ( x > 0 ) y > 0,
    if ( y > 0 ) x > 0,
    if ( A == 'a' ) B == 'b',
    if ( A == 'b' ) y > 3
    )
)

# substitute pure numerical variable
substValue(E,'z',10)
# substitute pure categorical variable
substValue(E,'A','a1')
# substitute variable appearing in logical constraints
substValue(E,'x',3)



}
\seealso{
  \code{\link{eliminate}}
}

