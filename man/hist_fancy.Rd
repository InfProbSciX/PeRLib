% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist_fancy.R
\name{hist_fancy}
\alias{hist_fancy}
\title{Make a pretty Multiple Histogram}
\usage{
hist_fancy(...)
}
\arguments{
\item{...}{Other arguments for the qplot, such as xlim, ylim, xlab, ylab, main, etc.}

\item{Named_Vectors}{Each vector is used to draw a multiple histogram with each label matching the corresponding vector name.}

\item{Unnamed_Vectors}{are named x{number}.}

\item{fill}{Optional character vector specifying fill colours, as large as the number of vectors fed into the function.}
}
\description{
\code{hist_fancy} Extends the hist() function by using ggplot2 for making a pretty histogram. The function calls ggplot2::qplot to make multiple histograms, so arguments will be the same as the hist or the qplot. A qplot object is returned so it's possible to add geoms to it.
}
\examples{
hist_fancy(rnorm(1000))
hist_fancy(x = rnorm(1000), main = "Histogram", xlab = "Variables")
hist_fancy(x = rnorm(1000), y = rnorm(1000, 2), z = rnorm(1000, 4), alpha = 0.9)
}
