\name{iter.backtracker}
\alias{iter.backtracker}
\title{iterate over all solutions of a \link{backtracker}...}
\usage{\method{iter}{backtracker}(x, ...)
}
\description{iterate over all solutions of a \code{\link{backtracker}}}
\details{iterate over all solutions of a \code{\link{backtracker}}
This method is identical to calling \code{$searchNext} on a \code{backtracker}. Please note that iterating
a backtracker changes the state of a backtracker.}
\value{backtracker iterator}
\seealso{\code{iter} from the package iterators}
\arguments{\item{x}{\code{\link{backtracker}} object}
\item{...}{extra parameters that will given to the \code{searchNext()} function}
}

