## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  # function that checks data for internal consistency and returns a preprocessed table
#  df <- bistablehistory::preprocess_data(br_single_subject,
#                                         state="State",
#                                         duration="Duration",
#                                         run="Block")
#  
#  # data for Stan model
#  stan_data <- list(
#    # complete time-series
#    rowsN = nrow(df),
#    duration = df$duration,
#    istate = df$istate,
#    is_used = df$is_used,
#    run_start = df$run_start,
#    session_tmean = df$session_tmean,
#  
#    # only valid clear percepts
#    clearN = sum(df$is_used),
#    clear_duration = df$duration[df$is_used == 1],
#  
#    # history parameters, all fixed to default values
#    history_starting_values = c(0, 0),
#    mixed_state = 0.5
#  )

## ----eval=FALSE---------------------------------------------------------------
#  # compile the model
#  model <- cmdstanr::cmdstan_model("example.stan")
#  
#  # sample model
#  fit <- model$sample(data=stan_data, chains=1)
#  
#  # extract posterior samples for tau parameter
#  tau <- fit$draws(variables = "tau")

