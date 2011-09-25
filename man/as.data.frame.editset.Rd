\name{as.data.frame.editset}
\alias{as.data.frame.editset}
\title{Coerce an editarset to a \code{data.frame}}
\usage{
  \method{as.data.frame}{editset} (x, ...)
}
\arguments{
  \item{x}{\code{\link{editset}} object}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  data.frame with columns 'name', 'edit' and 'description'.
}
\description{
  Coerces an editset to a \code{data.frame}.
}
\seealso{
  \code{\link{as.character.editarray}}
}

