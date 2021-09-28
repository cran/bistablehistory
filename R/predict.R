#' Computes predicted dominance phase durations using posterior predictive distribution.
#'
#' Computes predicted dominance phase durations using fitted model.
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to NULL (no CI).
#' @param ... Unused
#'
#' @return If \code{summary=FALSE}, a numeric matrix iterationsN x clearN.
#' If \code{summary=TRUE} but \code{probs=NULL} a vector of mean predicted durations.
#' If \code{summary=TRUE} and \code{probs} is not \code{NULL}, a data.frame
#' with a column _"Predicted"_ (mean) and a column for each specified quantile.
#'
#' @importFrom dplyr bind_cols
#' @importFrom rlang .data
#' @importFrom rstan extract
#' @importFrom stats quantile predict
#' @importFrom tibble tibble as_tibble
#'
#' @method predict cumhist
#' @export
#'
#' @seealso \code{\link{fit_cumhist}}
#' @examples
#' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' predicted <- predict(br_fit)
#'
#' # full posterior prediction samples
#' predictions_samples <- predict(br_fit, summary=FALSE)
#' }
predict.cumhist <-  function(object, summary=TRUE, probs=NULL, ...) {
  if (is.null(object$stanfit)) stop("The object has no fitted stan model")

    # extracting parameters
  lm_params <- rstan::extract(object$stanfit, pars="lm_param")$lm_param

  if (object$family == "gamma") {
    predictions <- exp(lm_params[, 1, ]) * exp(lm_params[, 2, ])
  }
  else if (object$family == "lognormal") {
    predictions <- exp(exp(lm_params[, 1, ]) + lm_params[, 2, ] / 2)
  }
  else if (object$family == "normal") {
    predictions <- lm_params[, 1, ]
  }

  # raw samples
  if (!summary) return(predictions)

  # means
  predictions_mean <- apply(as.matrix(predictions), MARGIN=2, FUN=mean)
  if (is.null(probs)) return(predictions_mean)

  # full summary
  tibble::tibble(Predicted = predictions_mean) %>%
    dplyr::bind_cols(tibble::as_tibble(t(apply(as.matrix(predictions), MARGIN=2, FUN=quantile, probs=probs))))
}
