% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Generics.R
\name{getTable}
\alias{getTable}
\title{\code{getTable} -- Get the data.frame from a discrete life table}
\usage{
getTable(object)
}
\arguments{
\item{object}{A life table object}
}
\value{
a data.frame with to collumns -- One representing the ages ans an other one representing the Lx's
}
\description{
This method gets you the data.frame coresponding to the life table passed as the \code{object} parameter. Be carefull, because life tables could be defined in a continuous way, and in this case this method wont work, but you could rely on the underlying cdf that you can get via \code{\link{getCdf}}.
}
\examples{
getTable(th002d)
getTable(tf002v)
\dontrun{
getTable(expolaw)
}
}
\seealso{
\code{\link{Lx}}, \code{\link{getAgeMax}}, \code{\linkS4class{TableMort}}, \code{\link{getCdf}}
}
