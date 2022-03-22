#' Computes an efficient approximate leave-one-out
#' cross-validation via loo library. It can be used
#' for a model comparison via loo::loo_compare() function.
#'
#' @param x A [cumhist][cumhist-class()] object
#' @param ... unused
#'
#' @return A named list, see [loo::loo()] for details.
#'
#' @importFrom rstan loo
#' @method loo cumhist
#' @export loo
#' @export
#'
#' @examples
#' data(br_singleblock)
#' \donttest{
#' gamma_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' loo_gamma <- loo(gamma_fit)
#' }
loo.cumhist <- function(x, ...) {
  rstan::loo(x$stanfit, cores = future::availableCores())
}
