% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posiciones.R
\name{find_rare_position}
\alias{find_rare_position}
\title{Find uncommon holdings}
\usage{
find_rare_position(position, cut = 0)
}
\arguments{
\item{position}{A data.frame object of holdings expected to be obtained from \code{\link{get_position}}}

\item{cut}{A numeric value indicating a cut value to filter using the median percentage of the total}
}
\value{
A data.frame object containing the holdings that have median percentage of the total within clients of the same
strategy below the selected cut value
}
\description{
Finds the holdings that have median below cut (defaults to zero) among clientes with the same strategy
}
\details{
The function computes the median percentage of the total holdings for each strategy and the filters the full
holdings information to return whose median is below the cut
}
\examples{
find_rare_position(get_position("2015-12-31"))
}
