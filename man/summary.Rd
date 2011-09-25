\name{summary.editmatrix}
\alias{summary.editarray}
\alias{summary.editmatrix}
\alias{summary.editset}
\alias{summary.errorLocation}
\title{Summarize}
\usage{
  \method{summary}{editmatrix} (object, useBlocks = TRUE,
    ...)

  \method{summary}{editarray} (object, useBlocks = TRUE,
    ...)

  \method{summary}{editset} (object, useBlocks = TRUE, ...)

  \method{summary}{errorLocation} (object, ...)
}
\arguments{
  \item{object}{an R object}

  \item{useBlocks}{\code{logical} Summarize each block?}

  \item{...}{Arguments to pass to or from other methods}
}
\description{
  Summarizes an object

  Summary of editarray

  Summary of editset

  Generates an object of class 'locationsummary' for which
  a print method is implemented.
}
\details{
  The return value is object of class 'editsummary' for
  which a print method is implemented.
}
\examples{
# Using a character vector to define contraints
E <- editmatrix(c("x+3*y==2*z", "x==z"))
print(E)

# Using a expression vector to define contraints
E <- editmatrix(expression(x+3*y==2*z, x==z))
print(E)

# an editmatrix also has a summary method:
summary(E)

# select rows from an editmatrix:
E <- editmatrix(c("x+3*y==2*z", "x >= z"))
E[getOps(E) == "=="]


#Using data.frame to define constraints
E.df <- data.frame(
    name =c("A","B","C"),
    edit = c("x == y",    
            "z + w == y + x",
            "z == y + 2*w"),
    description = c(
            "these variables should be equal","","")

)
print(E.df)

E <- editmatrix(E.df)
print(E)

# Here is the prototypical categorical edit: men cannot be pregnant.
E <- editarray(c(
    "gender \\\%in\\\% c('male','female')",
    "pregnant \\\%in\\\% c('yes','no')",
    "if( gender == 'male' ) pregnant == 'no'"
    )
)
E

# an editarray has a summary method:
summary(E)

# A yes/no variable may also be modeled as a logical:
editarray(expression(
    gender \\\%in\\\% c('male','female'),
    pregnant \\\%in\\\% c(TRUE, FALSE),
    if( gender == 'male' ) pregnant == FALSE
    )
)

# or, shorter: 
editarray(c(
    "gender \\\%in\\\% c('male','female')",
    "pregnant \\\%in\\\% c(TRUE, FALSE)",
    "if( gender == 'male' ) !pregnant"
    )
)

# the \\\%in\\\% statement may be used at will
editarray(c(
    "gender \\\%in\\\% c('male','female')",
    "pregnant \\\%in\\\% c(TRUE, FALSE)",
    "positionInHousehold \\\%in\\\% c('marriage partner', 'child', 'other')",
    "maritalStatus \\\%in\\\% c('unmarried','married','widowed','divorced')",
    "if( gender == 'male' ) !pregnant",
    "if( maritalStatus \\\%in\\\% c('unmarried','widowed','divorced')) !positionInHousehold \\\%in\\\% c('marriage partner','child')"
    )
)





# edits can be read from a vector of expressions 
E <- editset(expression(
    if ( x > 0 ) y > 0,
    x + y == z,
    A \%in\% letters[1:2],
    B \%in\% letters[2:3],
    if ( A == 'a') B == 'b',
    if ( A == 'b') x >= 0,
    u + v == w,
    if ( u == 0 ) w >= 0
))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')


## see also editfile
E <- editfile(system.file('script/edits/mixedits.R',package='editrules'))
E
summary(E)
as.data.frame(E)
getVars(E)
getVars(E,type='cat')
getVars(E,type='num')




# an editmatrix and some data:
E <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0"))

dat <- data.frame(
    x = c(1,-1,1),
    y = c(-1,1,1),
    z = c(2,0,2))

# localize all errors in the data
err <- localizeErrors(E,dat)

summary(err)

# what has to be adapted:
err$adapt
# weight, number of equivalent solutions, timings,
err$status


## Not run

# Demonstration of verbose processing
# construct 2-block editmatrix
F <- editmatrix(c(
    "x + y == z",
    "x > 0",
    "y > 0",
    "z > 0",
    "w > 10"))
# Using 'dat' as defined above, generate some extra records
dd <- dat
for ( i in 1:5 ) dd <- rbind(dd,dd)
dd$w <- sample(12,nrow(dd),replace=TRUE)

# localize errors verbosely
(err <- localizeErrors(F,dd,verbose=TRUE))

# printing is cut off, use summary for an overview
summary(err)

# or plot (not very informative in this artificial example)
plot(err)

## End(Not run)


# Example with different weights for each record
E <- editmatrix('x + y == z')
dat <- data.frame(
    x = c(1,1),
    y = c(1,1),
    z = c(1,1))

# At equal weights, both records have three solutions (degeneracy): adapt x, y or z:
localizeErrors(E,dat)$status

# Set different weights per record (lower weight means lower reliability):
w <- matrix(c(
    1,2,2,
    2,2,1),nrow=2,byrow=TRUE)

localizeErrors(E,dat,weight=w)


# an example with categorical variables
E <- editarray(c(
    "age \\\%in\\\% c('under aged','adult')",
    "maritalStatus \\\%in\\\% c('unmarried','married','widowed','divorced')",
    "positionInHousehold \\\%in\\\% c('marriage partner', 'child', 'other')",
    "if( age == 'under aged' ) maritalStatus == 'unmarried'",
    "if( maritalStatus \\\%in\\\% c('married','widowed','divorced')) !positionInHousehold \\\%in\\\% c('marriage partner','child')"
    )
)
E

#
dat <- data.frame(
    age = c('under aged','adult','adult' ),
    maritalStatus=c('married','unmarried','widowed' ), 
    positionInHousehold=c('child','other','marriage partner')
)
dat
localizeErrors(E,dat)
# the last record of dat has 2 degenerate solutions. Running  the last command a few times
# demonstrates that one of those solutions is chosen at random.

# Increasing the weight of  'positionInHousehold' for example, makes the best solution
# unique again
localizeErrors(E,dat,weight=c(1,1,2))




}

