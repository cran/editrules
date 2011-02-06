\name{iter.choicepoint}
\alias{iter.choicepoint}
\title{iterate over all solutions of a \link{choicepoint}...}
\usage{\method{iter}{choicepoint}(x, ...)
}
\description{iterate over all solutions of a \code{\link{choicepoint}}}
\details{iterate over all solutions of a \code{\link{choicepoint}}
This method is identical to calling \code{$searchNext} on a \code{choicepoint}. Please note that iterating
a choicepoint changes the state of a choicepoint.}
\value{choicepoint iterator}
\seealso{\code{iter} from the package iterators}
\arguments{\item{x}{\code{\link{choicepoint}} object}
\item{...}{extra parameters that will given to the \code{searchNext()} function}
}

