% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scores.R
\name{elementaryscores}
\alias{elementaryscores}
\alias{es_expect}
\alias{es_mean}
\alias{es_median}
\alias{es_prob}
\alias{es_quant}
\title{Elementary score functions}
\usage{
es_expect(t, x, y, level, right = FALSE)

es_mean(t, x, y, right = FALSE)

es_median(t, x, y, right = FALSE)

es_prob(t, x, y, right = FALSE)

es_quant(t, x, y, level, right = FALSE)
}
\arguments{
\item{t}{an object convertible by \code{as.vector}
containing treshold values.}

\item{x}{an object convertible by \code{as.matrix}
containing forecasts (dim: #observations,
#forecast methods).}

\item{y}{an object convertible by \code{as.vector}
containing observations.}

\item{level}{numerical, a single value between 0 and 1.}

\item{right}{logical, indicating if the scores are
right-continuous in \code{t}.}
}
\value{
Returns an array with dimension
 (#thresholds, #observations, #forecast methods).
}
\description{
Functions for the computation of the elementary scores.
}
