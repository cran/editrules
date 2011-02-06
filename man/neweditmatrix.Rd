\name{neweditmatrix}
\alias{neweditmatrix}
\title{Create an editmatrix object from its constituing attributes.}
\usage{neweditmatrix(mat, editrules, edits, ops, C)
}
\description{Create an \code{editmatrix} object from its constituing attributes.}
\details{This function is for internal purposes, please use \code{\link{editmatrix}} for creating an editmatrix object.}
\value{an S3 object of class \code{editmatrix}}
\arguments{\item{mat}{An object of class \code{matrix}}
\item{editrules}{the editrules \code{data.frame}}
\item{edits}{The parsed edits}
\item{ops}{a character vector with the comparison operator of every edit.}
\item{C}{\code{numeric} constant vector}
}

