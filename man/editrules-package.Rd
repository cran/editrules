\name{editrules-package}
\alias{editrules-package}
\alias{editrules-package}
\title{Facilitates reading and manipulation of linear...}
\description{Facilitates reading and manipulation of linear
(in)equalities, converting them into matrix format (and vice versa)
and checking if data are valid according to these rules. Sets of
linear edits can be manipulated using Fourier-Motzkin elimination
and variable substitution and solving error localization problems
based on the generalized principle of Felligi and Holt.}
\details{The package \code{editrules} facilitates the reading/parsing of so called "edit rules" or constraints. 
Edit rules are linear (in)equalities used for checking
statistical data. 
Often the specification of edit rules and their implementation in software is cumbersome. In many cases a restriction matrix 
is created manually. With \code{editrules} these rules can be specified in R syntax and can be managed and documented.
The documented form can be easily translated into a constraint matrix form, useful for automatic detection and correction 
methods.

If an observation passes the edit rules it is considered valid or plausible. 
If an observation doesn't pass the rules then it should be further checked and eventually edited.
Important for the (automatic) editing proces are the rules that are violated. \code{editrules} helps to detect which rules are 
violated and which variables are involved in the violated edits.


The function \code{\link{editmatrix}} reads linear (in)equalities.
Typical usage is:}
\docType{package}
\examples{# load a data.frame with the edit rules
E.df <- data.frame(
    name =c("A","B","C"),
    edit = c("x == y",    
            "z + w == y + x",
            "z == y + 2*w"),
    description = c(
            "these variables should be equal","","")

)

print(E.df)
# Create the editmatrix from the editrules
E <- editmatrix(E.df)
print(E)

#data that should conform to the constraints
dat <- data.frame( w = c(0,0,0)
                 , x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
print(dat)

# valid rows?
checkRows(E, dat)

#ok, lets show the errors
violatedEdits(E,dat)

#or, lets list the contraints that were violated
listViolatedEdits(E,dat)}

