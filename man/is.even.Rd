% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.evenodd.R
\name{is.even}
\alias{is.even}
\title{Check if a vector of numbers is Even}
\usage{
is.even(x, n = FALSE)
}
\arguments{
\item{x}{A numeric vector.}

\item{n}{An integer specifying if a number of even numbers should be returned.}
}
\description{
\code{is.even} Returns a vector of TRUE/FALSEs by applying x%2 to a vector x.
}
\examples{
is.even(1:5)

## FALSE  TRUE FALSE  TRUE FALSE
}
