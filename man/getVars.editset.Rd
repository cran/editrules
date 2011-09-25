\name{getVars.editset}
\alias{getVars.editset}
\title{get variable names in editset}
\usage{
  \method{getVars}{editset} (E,
    type = c("all", "num", "cat", "mix", "dummy"), ...)
}
\arguments{
  \item{type}{which variables? \code{all} means all (except
  dummies), \code{num} means all numericals, \code{cat}
  means all categoricals, \code{mix} means those numericals
  appearing in a logical constraint and \code{dummy} means
  dummy variables connecting the logical with numerical
  constraints.}

  \item{dummies}{Also return dummy variables used in
  \code{E$mixcat} (only if \code{type} is \code{all} or
  \code{cat})}
}
\description{
  get variable names in editset
}
\keyword{internal}

