% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{kde}
\alias{kde}
\title{Kinetic-depth effect data}
\format{
A data frame with 38698 rows and 6 variables:
\describe{
\item{Observer}{Participant ID.}
\item{Display}{Display, all rows contain \code{"KD"}}
\item{Block}{Run / block index.}
\item{Time}{Time relative to the run onset in \emph{seconds}}
\item{State}{Factor with levels \code{"Left"}, \code{"Right"} (clear states), and \code{"Mixed"}}.
\item{Duration}{Duration of a dominance phase in \emph{seconds}. Note that the duration for the last dominance phase is curtailed and, therefore, set to zero.}
}
}
\source{
\doi{10.1167/11.10.12}
}
\usage{
kde
}
\description{
Dataset on kinetic-depth effect for eleven participants.
}
\keyword{datasets}
