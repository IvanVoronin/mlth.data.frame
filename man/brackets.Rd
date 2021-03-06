% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brackets.R
\name{[[.mlth.data.frame}
\alias{[[.mlth.data.frame}
\alias{$.mlth.data.frame}
\alias{namesList}
\alias{selectByList}
\title{Extract or replace Parts of a Multi-Header Data Frame}
\usage{
\method{[[}{mlth.data.frame}(x, i)

\method{$}{mlth.data.frame}(object, name)

namesList(x)

selectByList(x, N)
}
\arguments{
\item{x, object}{a \code{mlth.data.frame} object.}

\item{i, j}{indices specifying elements to extract. 
Details in \link[base]{Extract} and \code{\link[base]{Extract.data.frame}}.
The list structure can be used to extract or re-arrange a multi-headed data frame,
see Details and Examples.}

\item{name}{a literal character string (use \link{backtick} when the name includes
spaces or special characters).}

\item{N}{a list which defines the \code{mlth.data.frame} structure to extract.
See Details section.}
}
\description{
The methods to extract parts of a multi-headed data frame.
Work the same way as the \code{\link{data.frame}}. The \code{[<-} and \code{[[<-}
are to be implemented.
}
\details{
The function \code{namesList} returns the list representing the structure of
a \code{mlth.data.frame} object. The variables are represented by their names
(single character string or vector), the sub-tables are represented by named
list or character vector (when a sub-table does not include another sub-table.
See Examples.

The function \code{selectByList} selects the parts of a \code{mlth.data.frame}
corresponding to the list structure. \code{[[} and \code{[} call \code{selectByList}
when \code{i} is a list.
}
\examples{
L <- mlth(X = c('A', 'B', 'C'),
		       Y = list(N = 1:3, M = 4:6))

L['Y']
L[['Y']]
L$Y ## Same as L[['Y']]
L$Y$M

L[1:2, ] ## Select rows

## Select using a list structure
namesList(L)
L[[list(Y = c('N', 'M', 'N'), 'X')]]
selectByList(L,list(Y=c('N','M','N'),'X')) # Same
L[1:2, list(Y = c('N', 'M', 'N'), 'X')]

}
