% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_history.R
\name{predict_history}
\alias{predict_history}
\title{Computes predicted cumulative history using posterior predictive distribution.}
\usage{
predict_history(
  object,
  history_type,
  summary = TRUE,
  probs = NULL,
  full_length = TRUE,
  ...
)
}
\arguments{
\item{object}{An object of class \link[=cumhist-class]{cumhist}}

\item{history_type}{\code{"1"} or \code{"2"} for cumulative history for the first or second perceptual
states (with indexes 1 and 2, respectively),
\code{"dominant"} or \code{"suppressed"} for cumulative history for states that either dominant
or suppressed during the following phase, \code{"difference"} for difference between suppressed
and dominant. See cumulative history vignette for details.}

\item{summary}{Whether summary statistics should be returned instead of
raw sample values. Defaults to \code{TRUE}}

\item{probs}{The percentiles used to compute summary, defaults to NULL (no CI).}

\item{full_length}{Only for \code{summary = TRUE}, whether the summary table should
include rows with no predictions. I.e., rows with mixed phases, first/last dominance
phase in the run, etc. See \code{\link[=preprocess_data]{preprocess_data()}}. Defaults to \code{TRUE}.}

\item{...}{Unused}
}
\value{
If \code{summary=FALSE}, a numeric matrix iterationsN x clearN.
If \code{summary=TRUE} but \code{probs=NULL} a vector of requested cumulative history values.
If \code{summary=TRUE} and \code{probs} is not \code{NULL}, a data.frame
with a column \emph{"Predicted"} (mean) and a column for each specified quantile.
}
\description{
Computes predicted cumulative history using fitted model. This is just a wrapper
for \code{predict(object, summary, probs, full_length, predict_history=history_type)}.
}
\examples{
\donttest{
br_fit <- fit_cumhist(br_singleblock, state = "State", duration = "Duration")
history_difference_summary <- predict_history(br_fit, "difference")

# full posterior prediction samples
history_difference <- predict_history(br_fit,
                                      "difference",
                                      summary = FALSE,
                                      full_length = TRUE)
}
}
\seealso{
\code{\link{fit_cumhist}}, \code{\link{predict.cumhist}}
}
