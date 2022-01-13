## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----minimal example duration, warning = FALSE, message = FALSE---------------
library(bistablehistory)

data(br_singleblock)
gamma_fit <- fit_cumhist(br_singleblock,
                         state="State",
                         duration="Duration",
                         refresh=0)

## ----eval=FALSE---------------------------------------------------------------
#  gamma_fit <- fit_cumhist(br_singleblock,
#                          state="State",
#                          onset="Time")

## -----------------------------------------------------------------------------
history_tau(gamma_fit)

## -----------------------------------------------------------------------------
historyef(gamma_fit)

## ----eval=FALSE---------------------------------------------------------------
#  gamma_fit <- fit_cumhist(br_singleblock,
#                          state="State",
#                          duration="Duration",
#                          control=list(max_treedepth = 15,
#                                       adapt_delta = 0.99))

## ----eval=FALSE---------------------------------------------------------------
#  gamma_fit <- fit_cumhist(br_single_subject,
#                          state="State",
#                          onset="Time",
#                          run="Block")

## ----eval=FALSE---------------------------------------------------------------
#  gamma_fit <-  fit_cumhist(kde_two_observers,
#                            state="State",
#                            duration="Duration",
#                            random_effect="Observer",
#                            run="Block")

## ----eval=FALSE---------------------------------------------------------------
#  H <- extract_history(gam_fit)

## ----eval=FALSE---------------------------------------------------------------
#  df <- compute_history(br_singleblock,
#                        state="State",
#                        duration="Duration",
#                        tau=1,
#                        mixed_state=0.5,
#                        history_init=0)

