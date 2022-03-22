#' Computes widely applicable information criterion
#' (WAIC).
#'
#' @description Computes widely applicable information criterion
#' via [loo][loo-package] library. It can be used for a model comparison via
#' [loo::loo_compare()][loo::loo_compare] function.
#'
#' @param x A [cumhist][cumhist-class()] object.
#' @param ... Additional arguments (unused)
#'
#' @return A named list, see [loo::waic()] for details.
#'
#' @importFrom loo extract_log_lik waic
#'
#' @method waic cumhist
#' @export
#'
#' @examples
#'  \donttest{
#' data(br_singleblock)
#' gamma_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' waic_gamma <- waic(gamma_fit)
#' normal_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration", family="normal")
#' waic_normal <- waic(normal_fit)
#' loo::loo_compare(waic_gamma, waic_normal)
#' }
waic.cumhist <- function(x, ...) {
  log_lik <- loo::extract_log_lik(x$stanfit, "log_lik", merge_chains = FALSE)
  loo::waic(log_lik)
}
