\name{as.character.editarray}
\alias{as.character.editarray}
\title{Coerce an editarray to \code{character}}
\usage{
  \method{as.character}{editarray} (x, useIf = TRUE,
  datamodel = TRUE, ...)
}
\arguments{
  \item{x}{editarray object}

  \item{useIf}{\code{logical}. Use if( <condition> )
  <statement> or !<condition> | <statement> ?}

  \item{datamodel}{\code{logical}. Include datamodel
  explicitly?}

  \item{...}{further arguments passed to or from other
  methods}
}
\description{
  Coerces an editarray to a \code{data.frame}. The
  resulting character vector can be reparsed to an
  editarray with \code{\link{editarray}}. The datamodel
  (the set of categories for every variable) is represented
  in the 'ind' attribute of an editarray. The character
  representation will contain a number of entries, named
  \code{d}\eqn{i}, of the form \code{"<variable> \%in\%
  c('<cat1>',...,'<catN>')"}.
}

