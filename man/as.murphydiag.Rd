% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coercion.R
\name{as.murphydiag}
\alias{as.murphydiag}
\alias{as.murphydiag.default}
\alias{as.murphydiag.data.frame}
\alias{as.murphydiag.murphydiag}
\alias{as.murphydiag.lm}
\alias{as.murphydiag.rq}
\alias{as.murphydiag.randomForest}
\alias{is.murphydiag}
\alias{simplifyFunctional}
\alias{generalizeFunctional}
\title{Coerce to a Murphy diagram}
\usage{
as.murphydiag(x, ...)

\method{as.murphydiag}{default}(x, y = NULL, type = NULL,
  level = NULL, m = NULL, xnames = NULL, ...)

\method{as.murphydiag}{data.frame}(x, y = NULL, type = NULL,
  level = NULL, m = NULL, xnames = NULL, ...)

\method{as.murphydiag}{murphydiag}(x, y = NULL, type = NULL,
  level = NULL, m = NULL, tol = sqrt(.Machine$double.eps), ...)

\method{as.murphydiag}{lm}(x, y = NULL, type = NULL, level = NULL,
  m = NULL, xnames = NULL, newdata = NULL, ...)

\method{as.murphydiag}{rq}(x, y = NULL, type = NULL, level = NULL,
  m = NULL, xnames = NULL, newdata = NULL, ...)

\method{as.murphydiag}{randomForest}(x, y = NULL, type = NULL,
  level = NULL, m = NULL, xnames = NULL, newdata = NULL, ...)

is.murphydiag(x)

simplifyFunctional(m, tol = sqrt(.Machine$double.eps))

generalizeFunctional(m)
}
\arguments{
\item{x}{any \R object.}

\item{y}{a numeric vector of response values to be predicted.}

\item{type}{a string specifying the type of forecast; one of \code{"prob"}, \code{"mean"}, \code{"expectile"}, \code{"median"}, \code{"quantile"}.}

\item{level}{a single numeric value for the level of the \code{"quantile"} or \code{"expectile"} functional.}

\item{m}{an object inheriting from the class \code{'murphydiag'}; alternative to \code{y}, \code{type}, \code{level}.}

\item{xnames}{a character vector of prediction names.}

\item{tol}{accuracy when comparing \code{y} and \code{level} in \code{'murphydiag'} objects.}

\item{newdata}{an optional data frame used as environment in which to evaluate \code{predict} methods of model objects.}
}
\value{
\code{as.murphydiag} returns a \code{'murphydiag'} object.
 
 \code{is.murphydiag} returns \code{TRUE} if its argument is a
 Murphy diagram, that is, has \code{"murphydiag"} among its classes,
 and \code{FALSE} otherwise.
 
 \code{simplifyFunctional} coerces \code{type} and \code{level} of a
 \code{'murphydiag'} object from \code{"quantile"} to \code{"median"},
 or \code{"expectile"} to \code{"mean"}, if possible. That is,
 when \code{level} is close enough to 0.5.
 
 \code{generalizeFunctional} coerces \code{type} of a \code{'murphydiag'}
 object from \code{"prob"} or \code{"mean"} to \code{"expectile"} with
 \code{level} at 0.5, or \code{"median"} to \code{"quantile"} with
 \code{level} at 0.5.
}
\description{
generic function with
a \code{default} method, and additional methods for classes
\code{\link[stats]{lm}}, \code{\link[randomForest]{randomForest}}, \code{\link[quantreg]{rq}} (from package \code{quantreg}).
}
\details{
In the default version, the user specifies all relevant information (forecasts, realizations, 
information on the type of forecast) manually. Furthermore, \code{murphydiag} accepts fitted model objects
from a few other packages:
\itemize{
\item \dQuote{lm}, see \code{\link[stats]{lm}}
\item \dQuote{rq}, see \code{\link[quantreg]{rq}}
\item \dQuote{randomForest}, see \code{\link[randomForest]{randomForest}}
}


 as.murphydiag.randomForest converts the classification problem to a binary problem with the first class treated as 1 and all other classes as 0
}
