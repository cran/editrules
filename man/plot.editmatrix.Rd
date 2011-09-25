\name{plot.editmatrix}
\alias{plot.editarray}
\alias{plot.editmatrix}
\title{Graphical representation of editmatrix}
\usage{
  \method{plot}{editmatrix} (x, nodetype = "all", rules =
  rownames(x), vars = getVars(x), ...)

  \method{plot}{editarray} (x, nodetype = "all", rules =
  rownames(x), vars = getVars(x), ...)
}
\arguments{
  \item{x}{object of class \code{\link{editmatrix}}}

  \item{nodetype}{plot edits, variables or both?}

  \item{rules}{selection of edits}

  \item{vars}{selection of variables}

  \item{...}{further arguments to be passed to plot.}
}
\description{
  This function computes the edge graph of an editmatrix
  and plots it with some sensible defaults. Each (labeled)
  vertex corresponds to an edit, while an edge indicates
  that its vertices share at least one variable. The
  linewidht of the plotted edge relates to the number of
  variables connecting the edits.

  plot method for editarray
}
\details{
  For more finetuning, see \code{\link{as.igraph}}, and
  \code{igraph.plotting}
}
\examples{

## Examples with linear (in)equality edits

# load predefined edits from package
data(edits)
edits

# convert to editmatrix
E <- editmatrix(edits)

## Not run:
# (Note to reader: the Not run directive only prevents the examle commands from running when package is built)

# Total edit graph
plot(E)

# Graph with dependent edits
plot(E, nodetype="rules")

# Graph with dependent variables
plot(E, nodetype="vars")

# Total edit graph, but with curved lines (option from igraph package)
plot(E, edge.curved=TRUE)


# graph, plotting just the connections caused by variable 't'
plot(E,vars='t')

## End(Not run) 


# extract total graph (as igraph object)
as.igraph(E)

# extract graph with edges related to variable 't' and 'ch'
as.igraph(E,vars=c('t','ch'))

# extract total adjacency matrix
adjacency(E)

# extract adjacency matrix related to variables t and 'ch'
adjacency(E,vars=c('t','ch'))

## Examples with categorical edits

# generate an editarray:
E <- editarray(c(
    "age \%in\% c('<15','16-65','>65')",
    "employment \%in\% c('unemployed','employed','retired')",
    "salary \%in\% c('none','low','medium','high')",
    "if (age == '<15') employment=='unemployed'",
    "if (salary != 'none') employment != 'unemployed'",
    "if (employment == 'unemployed') salary == 'none'"))


## Not run:
# plot total edit graph
plot(E)

# plot edit graph, just the connections caused by 'salary'
plot(E,vars='salary')

## End(Not run)

# extract edit graph
as.igraph(E)

# extract edit graph, just the connections caused by 'salary'
as.igraph(E,vars='salary')

# extract adjacency matrix
adjacency(E)

# extract adjacency matrix, only caused by 'employment'
adjacency(E,vars='employment')





}

