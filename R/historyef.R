#' Extract the history-effects estimates
#'
#' Extracts models population-level coefficients history-specific terms
#' for every modeled distribution parameter.
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#'
#' @return data.frame with values or summary
#'
#' @importFrom dplyr %>% group_by summarise ungroup mutate select left_join
#' @importFrom rlang .data
#' @importFrom rstan extract
#' @importFrom stats quantile
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr nest unnest
#' @export
#'
#' @examples
#' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' historyef(br_fit)
#' }
historyef <- function(object, summary=TRUE, probs=c(0.055, 0.945)){
  if (!is(object, "cumhist")) stop('The object must be of class "cumhist"')
  if (is.null(object$stanfit)) stop("The object has no fitted stan model")

  # distribution parameter names
  param_names <- list("gamma" = c("shape", "scale"),
                      "normal" = "mu",
                      "lognormal" = "mu")

  # extracting estimates
  bH <- rstan::extract(object$stanfit, pars="bH_mu")$bH_mu

  terms <-
    tibble::tibble(Estimate = c(bH),
                   DistributionParameter = rep(1:ncol(bH), each=nrow(bH))) %>%

    # adding distribution parameter names
    dplyr::mutate(DistributionParameter = factor(.data$DistributionParameter,
                                                 levels=1:dim(bH)[2],
                                                 labels=param_names[[object$family]]))

  if (!summary) return(terms)

  # mean
  avg_terms <-
    terms %>%
    dplyr::group_by(.data$DistributionParameter) %>%
    dplyr::summarise(Estimate = mean(.data$Estimate), .groups="drop")

  # quantiles
  if (!is.null(probs)){
    term_quantiles <-
      terms %>%
      dplyr::group_by(.data$DistributionParameter) %>%
      tidyr::nest() %>%
      dplyr::mutate(CI = purrr::map(.data$data, ~tibble::as_tibble(t(apply(as.matrix(.$Estimate), MARGIN=2, FUN=quantile, probs=probs))))) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols=.data$CI)

    avg_terms <- dplyr::left_join(avg_terms, term_quantiles, by="DistributionParameter")
  }
  avg_terms
}
