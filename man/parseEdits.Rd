\name{parseEdits}
\alias{parseEdits}
\title{Parse a character vector of edits...}
\usage{parseEdits(E)
}
\description{Parse a character vector of edits}
\details{This function wraps the native \code{\link{parse}} function in a \code{\link{tryCatch}}.
The function is \code{editrules} internal. It tries to give a meaningfull error message when
parsing fails for some reason.}
\value{The edits in \code{E} parsed to R expressions.}
\arguments{\item{E}{\code{character}}
}

