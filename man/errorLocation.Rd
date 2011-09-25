\name{errorLocation}
\alias{errorLocation}
\title{The errorLocation object}
\description{
  Object storing information on error locations in a
  dataset.
}
\details{
  The \code{errorlocation} objects consists of the
  following slots wich can be accessed with the dollar
  operator, just like with lists. Right now the only
  function creating such objects is
  \code{\link{localizeErrors}}

  \itemize{ \item{\code{adapt} a \code{logical} array where
  each row/column shows which record/variable should be
  adapted.} \item{\code{status} A \code{data.frame} with
  the same number of rows as \code{adapt}. It contains the
  following columns \itemize{ \item{\code{weight} weight of
  the found solution} \item{\code{degeneracy} number of
  equivalent solutions found} \item{\code{user} user time
  used to generate solution (as in \code{sys.time})}
  \item{\code{system} system time used to generate solution
  (as in \code{sys.time})} \item{\code{elapsed} elapsed
  time used to generate solution (as in \code{sys.time})}
  \item{\code{maxDurationExceeded} Was the maximum search
  time reached?} } } \item{\code{method} The error
  localization method used, can be "mip" or "localizer".}
  \item{\code{call} The R call to the function generating
  the object.} \item{\code{user} \code{character} user who
  generated the object.} \item{\code{timestamp}
  \code{character} timestamp.} }
}
\seealso{
  \code{\link{localizeErrors}}
}

