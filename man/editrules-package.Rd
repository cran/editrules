\name{editrules-package}
\alias{editrules-package}
\alias{editrules-package}
\title{Facilitates reading of linear (in)equalities and...}
\description{Facilitates reading of linear (in)equalities and
converting them to a matrix format (and vice versa)}
\details{Edit rules facilitates the reading/parsing of so called "edit rules" or constraints. These are linear (in)equalities used for checking
statistical data. If an observation (row) passes these rules it is considered valid or plausible. 
If an observation doesn't pass the rules then it should be checked and eventually edited.
Important for the (automatic) editing proces are the rules that are violated. 

The function \code{\link{editmatrix}} reads linear (in)equalities.
Typical usage is:}
\docType{package}
\examples{# load a data.frame with the edit rules
edtrules.csv <- 
'name , edit       , description
A , x == y         , "these variables should be equal"
B , z + w == y + x ,
C , z == y + 2*w   ,
'
edtrules <- read.csv(textConnection(edtrules.csv))			

# Create the editmatrix from the editrules
em <- editmatrix(edtrules)
print(em)

#data that should conform to the constraints
dat <- data.frame( w = c(0,0,0)
                 , x = c(0,2,1)
                 , y = c(0,0,1)
                 , z = c(0,1,1)
                 )
# valid rows?
checkRows(em, dat)

#ok, lets show the errors
errorMatrix(em,dat)

#or, lets list the contraints that were violated
listErrors(em,dat)}

