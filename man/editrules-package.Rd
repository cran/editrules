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
\details{The \code{editrules} package aims to provide an environment to conveniently define,
read and check linear (in)equality restrictions, perform common of rule sets
manipulations and offer error localization functionality based on the
(generalized) paradigm of Fellige Holt. This paradigm assumes
that errors are distributed randomly over the variables and there
is no detectable cause of error. It also decouples the detection of corrupt variables from their correction. 
For some types of error, such as sign flips,
typing errors or rounding errors, this assumption does not hold. These errors can be detected and are closely 
related to their resolution. The reader is referred to the package \code{deducorrect}
package for treating such errors. 

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

