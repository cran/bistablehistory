% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayes_R2.R
\name{bayes_R2}
\alias{bayes_R2}
\alias{bayes_R2.cumhist}
\title{Computes R-squared using Bayesian R-squared approach.}
\usage{
\method{bayes_R2}{cumhist}(object, summary = TRUE, probs = c(0.055, 0.945), ...)
}
\arguments{
\item{object}{An object of class \link[=cumhist-class]{cumhist}}

\item{summary}{Whether summary statistics should be returned instead of
raw sample values. Defaults to \code{TRUE}}

\item{probs}{The percentiles used to compute summary, defaults to 89\% credible interval.}

\item{...}{Unused.}
}
\value{
vector of values or a data.frame with summary
}
\description{
For detail refer to:
Andrew Gelman, Ben Goodrich, Jonah Gabry, and Aki Vehtari (2018).
R-squared for Bayesian regression models. The American Statistician
\doi{10.1080/00031305.2018.1549100} and
\url{https://avehtari.github.io/bayes_R2/bayes_R2.html}
}
\examples{
\donttest{
br_fit <- fit_cumhist(br_singleblock, state = "State", duration = "Duration")
bayes_R2(br_fit)
}
}
