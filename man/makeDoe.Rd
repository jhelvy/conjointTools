% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doe.R
\name{makeDoe}
\alias{makeDoe}
\title{Make a design of experiment}
\usage{
makeDoe(levels, varNames = NULL, type = "full", nTrials = NA)
}
\arguments{
\item{levels}{A vector of levels for each attribute.}

\item{varNames}{A vector of the names of each attribute in \code{levels} (defaults to \code{NULL}).}

\item{type}{The type of design. The default value of \code{"full"} returns a full factorial design. All other options return fractional factorial designs based on different criteria. The options \code{"D"}, \code{"A"}, and \code{"I"} return fractional factorial designs based on \code{"D"}, \code{"A"}, and \code{"I"} optimality metrics.}

\item{nTrials}{The number of trials to be used in a fractional factorial design. Defaults to \code{NA}, but must be a number less than the number of alternatives in the full factorial design if the \code{type} argument is anything other than \code{"full"}.}
}
\value{
Returns a full factorial or fraction factorial design of experiment based on the number of levels for each attribute.
}
\description{
Make a design of experiment
}
\examples{
\dontrun{
# Generate a full factorial design of experiment
doe <- makeDoe(levels = c(3, 3, 3))

# Generate a full factorial design of experiment about apples
doe <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "full"
)

# Generate a "D-optimal" fractional factorial design of experiment about apples
doe <- makeDoe(
    levels = c(3, 3, 3),
    varNames = c("price", "type", "freshness"),
    type = "D",
    nTrials = 15
)
}
}
