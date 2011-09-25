\name{editset}
\alias{editset}
\title{Create an editset which can contain a mix of categorical, numerical and mixededits}
\usage{
  editset(editrules, env = new.env())
}
\arguments{
  \item{editrules}{\code{data.frame} with (in)equalities
  written in R syntax, or alternatively a \code{character}
  or \code{expression} with (in)equalities written in R
  syntax}

  \item{env}{environment to parse categorical edits in}
}
\description{
  Mixed edits are most conveniently read from a free-format
  textfile, using \code{\link{editfile}}.
}
\details{
  NOTE: at the moment, functionality for mixed edit sets is
  limited and somewhat experimental.
}

