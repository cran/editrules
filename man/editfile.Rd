\name{editfile}
\alias{editfile}
\title{Read edits edits from free-form textfile}
\usage{
  editfile(file, type = c("all", "num", "cat", "mix"),
    expandEdits = FALSE, ...)
}
\arguments{
  \item{file}{name of text file to read in}

  \item{type}{type of edits to extract. Currently, only
  'num' (numerical), 'cat' (categorical) and 'all' are
  implemented.}

  \item{expandEdits}{should edits be expanded with
  \code{\link{expandEdits}}? (currently ignored)}

  \item{...}{extra parameters that will be passed to
  \code{expandEdits}}
}
\value{
  \code{editset} with all edits if \code{type=all},
  \code{\link{editarray}} if \code{type='cat'},
  \code{\link{editmatrix}} if \code{type='num'},
  \code{\link{editset}} with conditional edits if
  \code{type='mix'}.  If the return value is a \code{list},
  the elements are named \code{numedits} and
  \code{catedits}.
}
\description{
  This utility function allows for free editrule definition
  in a file. One can extract only the numerical
  (\code{type='num'}), only the categorical
  (\code{type='cat'}) or all editas (default) in which case
  a list is returned. The function first parses all
  assignments in the file. Every other statement is
  considered to be an edit.
}

