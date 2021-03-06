% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{optimist_wt}
\alias{optimist_wt}
\title{Halflife-weighted mean}
\usage{
optimist_wt(size, halflife)
}
\arguments{
\item{size}{The length of the array}

\item{halflife}{The parameter for geometric time-weighting}
}
\value{
A vector of length \code{size} with weights with total sum \code{1},
decaying geometrically with a factor of \code{1/halflife} and equal weight at the tail.
}
\details{
Uses geometric decay weighting where the last observations has weight \code{1/halflife}, with
geometric decay. The last \code{halflife} observations have weight \code{(1/halflife)^(size - 1)}.
}
