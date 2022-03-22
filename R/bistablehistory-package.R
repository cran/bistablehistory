#' Cumulative History Analysis for Bistable Perception Time Series
#'
#' @description Estimates cumulative history for time-series for continuously
#' viewed bistable perceptual rivalry displays. Computes cumulative history
#' via a homogeneous first order differential process. I.e., it assumes
#' exponential growth/decay of the history as a function time and perceptually
#' dominant state, Pastukhov & Braun (2011) \doi{10.1167/11.10.12}.
#' Supports Gamma, log normal, and normal distribution families.
#' Provides a method to compute history directly and example of using the
#' computation on a custom Stan code.
#'
#' @docType package
#' @name bistablehistory-package
#' @aliases bistablehistory
#' @useDynLib bistablehistory, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols row_number n mutate group_by ungroup summarise %>%
#' @importFrom stats coef predict quantile
#' @importFrom utils data
#' @importFrom rlang .data
#'
#' @seealso
#' \code{vignette("cumulative-history", package = "bistablehistory")}
#' \code{vignette("usage-examples", package = "bistablehistory")}
#' \code{vignette("writing-stan-code", package = "bistablehistory")}
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. https://mc-stan.org
#'
NULL
