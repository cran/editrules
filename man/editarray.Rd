\name{editarray}
\alias{editarray}
\title{Parse textual, categorical edit rules to an editarray}
\usage{
  editarray(editrules, sep = ":", env = parent.frame())
}
\arguments{
  \item{editrules}{\code{character} vector}

  \item{sep}{textual separator, to be used internally for
  separating variable from category names.}

  \item{env}{environment to evaluate the rhs of '==' or
  '\%in\%' in.}
}
\value{
  editarray
}
\description{
  Transforms a list of categorical edit rules to a boolean
  array representation. An editarry is used to store
  demands on purely categorical data.
}
\details{
  The purpose of this function is to turn human-readable
  demands on categorical data to a boolean array.
  Categorical edit rules state demands on a dataset in the
  form of a quoted R expression. Allowed statements include
  \code{if}, operators \code{\%in\%}, \code{==}, \code{!=},
  \code{||}, \code{&&} and brackets \code{()} and
  \code{\{\}}.

  The datamodel is derived from the edit set. A data model
  can be defined by simply adding univariate edits of the
  form

  \itemize{ \item{\code{"<variable> \%in\%
  c('<cat1>','<cat2>',...,'<catN>')"}} }

  Note the double quotes around the whole statement, and
  the single quotes around the category levels. The right
  hand side is evaluated when the editarray is generated,
  so it may also be the name of a previously defined
  variable. Also see the examples section.

  Relations between variables can be written in the form

  \itemize{ \item{\code{"if( <logical expression involving
  categorical variables> ) <logical expression involving
  categorical variables>"}} } See the example section for
  some coded examples.

  The result is an object of class \code{editarray}, which
  contains a \eqn{m\times n} boolean array, representing
  the multivariate edits. The columns are labeled with
  \code{<variable><sep><category>}, for example
  \code{gender:Male}. The column names represent the data
  model for the data to be treated, the entries represent
  the edit rules. For example. if the datamodel is
  \code{"gender \%in\% c('male','female')"} and
  \code{"pregnant \%in\% c(TRUE,FALSE)"}, the edit
  \code{"if(gender == 'male') pregnant == FALSE"} is
  represented by the boolean array
  \code{c(gender:male=TRUE, gender:female=FALSE,
  pregnant:TRUE=TRUE, pregnant:FALSE=FALSE)}.
}
\examples{

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




}
\seealso{
  \code{\link{editfile}}
}

