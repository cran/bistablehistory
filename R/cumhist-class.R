#' Class \code{cumhist}.
#'
#' Cumulative history model fitted to time-series data.
#'
#' @name cumhist-class
#' @aliases cumhist
#' @docType class
#'
#' @details
#' See \code{methods(class = "cumhist")} for an overview of available methods.
#'
#' @slot family A \code{string} with distribution family.
#' @slot data A \code{list} with preprocessed data.
#' @slot stanfit a \code{\link[rstan:stanfit-class]{stanfit}} object.
#'
#' @seealso
#'   \code{\link{fit_cumhist}}
NULL
