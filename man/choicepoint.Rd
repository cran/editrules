\name{choicepoint}
\alias{choicepoint}
\title{Choice point: a binary search program...}
\usage{choicepoint(isSolution, choiceLeft, choiceRight, list, ...)
}
\description{Choice point: a binary search program}
\details{\code{choicepoint} creates a binary search program that can be started by calling the \code{$searchNext} function
It walks a binary tree depth first. For all left nodes \code{choiceLeft} is evaluated, for all right nodes 
\code{choiceRight} is evaluated. A solution is found if \code{isSolution} evaluates to \code{TRUE}.
If \code{isSolution} evaluates to NULL it will continue to search deaper.
If \code{isSolution} evaluates to \code{FALSE} it stops at the current node and goes up the next search node

\subsection{Methods}{
\describe{
\item{\code{$searchNext(..., VERBOSE=FALSE)}}{Search next solution, can
be called repeatedly until there is no solution left. Named variables will be
added to the search environment, this feature can be used to direct the search
in subsequent calls to \code{searchNext}. VERBOSE=TRUE will print all
intermediate search steps and results. It can be used to debug the expressions
in the choicepoint}
\item{\code{$searchAll(..., VERBOSE=FALSE)}}{Return all solutions as a list}
\item{\code{$reset()}}{Resets the \code{choicepoint} to its initial state.}
}
}}
\value{choicepoint object, see Methods for a description of the methods}
\arguments{\item{isSolution}{\code{expression} that should evaluate to \code{TRUE} when a solution is found.}
\item{choiceLeft}{\code{expression} that will be evaluated for a left node}
\item{choiceRight}{\code{expression} that will be evaluated for a right node}
\item{list}{\code{list} with variables that will be added to the search environment}
\item{...}{named variables that will be added to the search environment}
}
\examples{cp <- choicepoint( isSolution= { 
                                 if (y==0) return(TRUE)
                                 if (x == 0) return(FALSE)
                               }
                 , choiceLeft = { x <- x - 1; y <- y}
                 , choiceRight = { y <- y - 1; x <- x}
                 , x=2
                 , y=1
                 )

cp$searchNext(VERBOSE=TRUE)
cp$searchNext(VERBOSE=TRUE)

# next search will return NULL because there is no more solution
cp$searchNext()


cp$reset()

# choicepoint also works with iterators
if (require(iterators)){
   it <- iter(cp)
   nextElem(it)
   #nextElem(it)
}}

