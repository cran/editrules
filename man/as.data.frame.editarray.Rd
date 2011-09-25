\name{as.data.frame.editarray}
\alias{as.data.frame.editarray}
\title{Coerce an editarray to a \code{data.frame}}
\usage{
  \method{as.data.frame}{editarray} (x, ...)
}
\arguments{
  \item{x}{\code{\link{editarray}} object}

  \item{...}{further arguments passed to or from other
  methods.}
}
\value{
  data.frame with columns 'name', 'edit' and 'description'.
}
\description{
  Coerces an editarray to a \code{data.frame}.
}
\seealso{
  \code{\link{as.character.editarray}}
}

